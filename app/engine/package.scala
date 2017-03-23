import cats.data.WriterT
import cats.{FlatMap, Functor, Semigroup}
import model.RoutingReason.RoutingReason
import model.{Location, Locations}
import play.api.libs.json.Json.JsValueWrapper
import play.api.libs.json.{JsObject, Json}
import play.api.mvc.{AnyContent, Request}
import uk.gov.hmrc.play.audit.AuditExtensions.auditHeaderCarrier
import uk.gov.hmrc.play.audit.model.ExtendedDataEvent
import uk.gov.hmrc.play.config.AppName
import uk.gov.hmrc.play.frontend.auth.AuthContext
import uk.gov.hmrc.play.frontend.auth.connectors.domain.Accounts
import uk.gov.hmrc.play.http.HeaderCarrier

import scala.concurrent.{ExecutionContext, Future}

package object engine {

  sealed trait TAuditInfo {
    lazy val transactionNames = Map(
      Locations.PersonalTaxAccount -> "sent to personal tax account",
      Locations.BusinessTaxAccount -> "sent to business tax account"
    )

    def toAuditEvent(location: Location)(implicit hc: HeaderCarrier, authContext: AuthContext, request: Request[AnyContent]): ExtendedDataEvent = {
      this match {
        case AuditInfo(routingReasons, ruleApplied, throttlingInfo) =>
          val accounts: Accounts = authContext.principal.accounts
          val accountMap = accounts.toMap
          val accountsAsJson: Seq[(String, JsValueWrapper)] = accountMap
            .map { case (k, v) => (k, Json.toJsFieldJsValueWrapper(v.toString)) }
            .toSeq
          val optionalAccounts: JsObject = Json.obj(accountsAsJson: _*)
          ExtendedDataEvent(
            auditSource = AppName.appName,
            auditType = "Routing",
            tags = hc.toAuditTags(transactionNames.getOrElse(location, "unknown transaction"), request.path),
            detail = Json.obj(
              "destination" -> location.url,
              "reasons" -> routingReasons.map { case (k, v) => k.key -> v.toString },
              "throttling" -> throttlingInfo.map(info => Map(
                "enabled" -> info.throttlingEnabled.toString,
                "sticky-routing-applied" -> info.stickyRoutingApplied.toString,
                "throttlingPercentage" -> info.throttlingPercentage.map(_.toString).getOrElse("-"),
                "throttled" -> info.throttled.toString,
                "destination-url-before-throttling" -> info.initialDestination.url,
                "destination-name-before-throttling" -> info.initialDestination.name
              )),
              "ruleApplied" -> ruleApplied
            ) ++ optionalAccounts
          )
      }
    }
  }

  case class AuditInfo(routingReasons: Map[RoutingReason, Boolean],
                       ruleApplied: Option[String],
                       throttlingInfo: Option[ThrottlingInfo]) extends TAuditInfo

  case class ThrottlingInfo(throttlingPercentage: Option[Int],
                            throttled: Boolean,
                            initialDestination: Location,
                            throttlingEnabled: Boolean,
                            stickyRoutingApplied: Boolean)

  object AuditInfo {

    val Empty = AuditInfo(routingReasons = Map.empty, ruleApplied = None, throttlingInfo = None)

    def apply(routingReasons: Map[RoutingReason, Boolean]): AuditInfo = AuditInfo(routingReasons = routingReasons, ruleApplied = None, throttlingInfo = None)

    implicit val mergeAudit: Semigroup[AuditInfo] = new Semigroup[AuditInfo] {
      override def combine(x: AuditInfo, y: AuditInfo): AuditInfo = x.copy(routingReasons = x.routingReasons ++ y.routingReasons)
    }
  }

  implicit def futureFunctor(implicit ec: ExecutionContext): Functor[Future] = new Functor[Future] {
    override def map[A, B](fa: Future[A])(f: (A) => B): Future[B] = fa.map(f)
  }

  implicit def futureFlatMap(implicit ec: ExecutionContext): FlatMap[Future] = new FlatMap[Future] {
    override def flatMap[A, B](fa: Future[A])(f: (A) => Future[B]): Future[B] = fa flatMap f

    override def tailRecM[A, B](a: A)(f: (A) => Future[Either[A, B]]): Future[B] = f(a).map {
      case Right(v) => v
    }

    override def map[A, B](fa: Future[A])(f: (A) => B): Future[B] = fa map f
  }

  type ConditionResult = WriterT[Future, AuditInfo, Boolean]
  type RuleResult = WriterT[Future, AuditInfo, Option[Location]]
  type EngineResult = WriterT[Future, AuditInfo, Location]
  val emptyRuleResult: RuleResult = WriterT(Future.successful(AuditInfo.Empty, None))

}

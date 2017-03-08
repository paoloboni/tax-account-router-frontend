import cats.data.WriterT
import cats.{FlatMap, Functor, Semigroup}
import model.Location
import model.RoutingReason.RoutingReason

import scala.concurrent.{ExecutionContext, Future}

package object engine {

  case class AuditInfo(routingReasons: Map[RoutingReason, Boolean], ruleApplied: Option[String], throttlingInfo: Option[ThrottlingInfo])
  case class ThrottlingInfo(throttlingPercentage: Option[Int], throttled: Boolean, initialDestination: Location, throttlingEnabled: Boolean, stickyRoutingApplied: Boolean)

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

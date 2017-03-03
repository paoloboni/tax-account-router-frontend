import cats.Semigroup
import cats.data.Writer
import model.Location
import model.RoutingReason.RoutingReason

import scala.concurrent.Future

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

  type ConditionResult = Future[Writer[AuditInfo, Boolean]]
  type RuleResult = Future[Writer[AuditInfo, Option[Location]]]
  type EngineResult = Future[Writer[AuditInfo, Location]]
  val emptyRuleResult: RuleResult = Future.successful(Writer(AuditInfo.Empty, None))

}

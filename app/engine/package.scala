import cats.Semigroup
import cats.data.Writer
import model.Location
import model.RoutingReason.RoutingReason

import scala.concurrent.Future

package object engine {

  case class AuditInfo(routingReasons: Map[RoutingReason, Boolean], ruleApplied: Option[String] = None)
  case class ThrottlingInfo(throttlingPercentage: Option[Int], throttled: Boolean, initialDestination: Location, throttlingEnabled: Boolean, stickyRoutingApplied: Boolean)

  object AuditInfo {
    implicit val mergeAuditInfo: Semigroup[AuditInfo] = new Semigroup[AuditInfo] {
      override def combine(x: AuditInfo, y: AuditInfo): AuditInfo = AuditInfo(x.routingReasons ++ y.routingReasons)
    }
  }

  type ConditionResult = Future[Writer[AuditInfo, Boolean]]
  type RuleResult = Future[Writer[AuditInfo, Option[Location]]]

}

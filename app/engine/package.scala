import cats.data.Writer
import model.Location
import model.RoutingReason.RoutingReason

import scala.concurrent.Future

package object engine {

  case class AuditInfo(routingReasons: Map[RoutingReason, Boolean])
  case class ThrottlingInfo(throttlingPercentage: Option[Int], throttled: Boolean, initialDestination: Location, throttlingEnabled: Boolean, stickyRoutingApplied: Boolean)

  type ConditionResult = Future[Writer[AuditInfo, Boolean]]
  type RuleResult = Future[Writer[AuditInfo, Option[Location]]]

}

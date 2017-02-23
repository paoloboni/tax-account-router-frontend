package v2

import cats.Semigroup
import cats.data.Writer
import model.RoutingReason.{RoutingReason, _}

import scala.util.Random

sealed trait Location

case object BusinessTaxAccount extends Location

case class RandomRuleContext(random: Random)

trait ConditionT[C] {
  self =>

  implicit val mergeAuditInfo: Semigroup[AuditInfo] = new Semigroup[AuditInfo] {
    override def combine(x: AuditInfo, y: AuditInfo): AuditInfo = AuditInfo(x.routingReasons ++ y.routingReasons)
  }

  val name = getClass.getName.split("\\$").last

  type Result = Writer[AuditInfo, Boolean]

  def apply(context: C): Result

  import scala.languageFeature.implicitConversions

  implicit def toConditionOps(condition: ConditionT[C]): ConditionOps = ConditionOps(condition)

  case class ConditionOps(condition: ConditionT[C]) {
    def &&(other: ConditionT[C]) = And(condition, other)
    def ||(other: ConditionT[C]) = Or(condition, other)
  }

}
case class Condition[C](f: C => Writer[AuditInfo, Boolean]) extends ConditionT[C] {
  override def apply(context: C): Result = f(context)
}

case class And[C](c1: ConditionT[C], c2: ConditionT[C]) extends ConditionT[C] {
  override def apply(context: C): Result = for {
    a <- c1(context)
    b <- c2(context)
  } yield a && b
}

case class Or[C](c1: ConditionT[C], c2: ConditionT[C]) extends ConditionT[C] {
  override def apply(context: C): Result = for {
    a <- c1(context)
    b <- c2(context)
  } yield a || b
}

case class Not[C](condition: ConditionT[C]) extends ConditionT[C] {
  override def apply(context: C): Result = condition(context).map(!_)
}

case class When[C](condition: ConditionT[C]) {
  def thenReturn(location: Location): RuleT = Rule(condition, location)
}

case class AuditInfo(routingReasons: Map[RoutingReason, Boolean])
case class ThrottlingInfo(throttlingPercentage: Option[Int], throttled: Boolean, initialDestination: Location, throttlingEnabled: Boolean, stickyRoutingApplied: Boolean)

trait RuleT {
  def evaluate(ruleContext: RandomRuleContext): Writer[AuditInfo, Option[Location]]
}

case class Rule[C](condition: ConditionT[C], location: Location) extends RuleT {
  override def evaluate(ruleContext: RandomRuleContext): Writer[AuditInfo, Option[Location]] = condition(ruleContext).mapBoth { case (auditInfo, result) =>
    if (result) (auditInfo, Some(location))
    else (auditInfo, None)
  }
}

case object AlwaysTrue extends ConditionT[RandomRuleContext] {
  override def apply(ruleContext: RandomRuleContext): Result = Writer(AuditInfo(Map(IS_A_VERIFY_USER -> true)), true)
}

case object AlwaysFalse extends ConditionT[RandomRuleContext] {
  override def apply(ruleContext: RandomRuleContext): Result = Writer(AuditInfo(Map(IS_A_GOVERNMENT_GATEWAY_USER -> false)), false)
}

case object SometimesTrue extends ConditionT[RandomRuleContext] {
  val conditionName = GG_ENROLMENTS_AVAILABLE

  override def apply(ruleContext: RandomRuleContext): Result = {
    val result = ruleContext.random.nextBoolean()
    Writer(AuditInfo(Map(conditionName -> result)), result)
  }
}

object Test extends App {

  val rc = RandomRuleContext(Random)

  assert(When(And(AlwaysTrue, Not(AlwaysFalse))) == When(AlwaysTrue && Not(AlwaysFalse)))
  val (a1, v1) = (When(AlwaysTrue && Not(AlwaysFalse)) thenReturn BusinessTaxAccount).evaluate(rc).run
  assert(a1 == AuditInfo(Map(IS_A_VERIFY_USER -> true, IS_A_GOVERNMENT_GATEWAY_USER -> false)))
  assert(v1 contains BusinessTaxAccount)

  val testRule = When(And(SometimesTrue, Or(AlwaysTrue, AlwaysFalse))) thenReturn BusinessTaxAccount

  val rcWithRandomReturningTrue = RandomRuleContext(new Random {
    override def nextBoolean() = true
  })
  val (a2, v2) = testRule.evaluate(rcWithRandomReturningTrue).run
  assert(a2 == AuditInfo(Map(GG_ENROLMENTS_AVAILABLE -> true, IS_A_VERIFY_USER -> true, IS_A_GOVERNMENT_GATEWAY_USER -> false)))
  assert(v2 contains BusinessTaxAccount)

  val rcWithRandomReturningFalse = RandomRuleContext(new Random {
    override def nextBoolean() = false
  })
  val (a3, v3) = testRule.evaluate(rcWithRandomReturningFalse).run
  assert(a3 == AuditInfo(Map(GG_ENROLMENTS_AVAILABLE -> false, IS_A_VERIFY_USER -> true, IS_A_GOVERNMENT_GATEWAY_USER -> false)))
  assert(v3 isEmpty)

  println(testRule)
}
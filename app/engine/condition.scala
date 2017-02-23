/*
 * Copyright 2017 HM Revenue & Customs
 *
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 *
 *     http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 */

package engine

import cats.Semigroup
import cats.data.Writer
import model.Location
import model.RoutingReason.RoutingReason
//import uk.gov.hmrc.play.http.logging.MdcLoggingExecutionContext.fromLoggingDetails

import scala.concurrent.Future

sealed trait Expr[Context, Result] {
  def evaluate(context: Context): Writer[AuditInfo, Result]
}

sealed trait Condition[C] extends Expr[C, Boolean] {

  implicit val mergeAuditInfo: Semigroup[AuditInfo] = new Semigroup[AuditInfo] {
    override def combine(x: AuditInfo, y: AuditInfo): AuditInfo = AuditInfo(x.routingReasons ++ y.routingReasons)
  }

  def evaluate(context: C): ConditionResult = this match {

    case Pure(f, auditType) =>
      for {
       r <- f(context)
      } yield Writer(AuditInfo(Map(auditType -> r)), r)

    case And(c1, c2) =>
      for {
        a <- c1.evaluate(context)
        b <- c2.evaluate(context)
      } yield for {
        a1 <- a
        b1 <- b
      } yield a1 && b1

    case Or(c1, c2) =>
      for {
        a <- c1.evaluate(context)
        b <- c2.evaluate(context)
      } yield for {
        a1 <- a
        b1 <- b
      } yield a1 || b1

    case Not(condition) =>
      for {
        c <- condition.evaluate(context)
      } yield for {
        c1 <- c
      } yield !c1
  }

  import scala.languageFeature.implicitConversions

  implicit val toConditionOps: ConditionOps = ConditionOps(this)

  case class ConditionOps(condition: Condition[C]) {
    def and(other: Condition[C]) = And(condition, other)
    def or(other: Condition[C]) = Or(condition, other)
  }

}

sealed trait RoutingReasonT { self: Condition[_] =>
  def auditType: Option[RoutingReasonT]
}

case class Pure[C](f: C => Future[Boolean], auditType: RoutingReason) extends Condition[C] with RoutingReasonT
case class And[C](c1: Condition[C], c2: Condition[C]) extends Condition[C]
case class Or[C](c1: Condition[C], c2: Condition[C]) extends Condition[C]
case class Not[C](condition: Condition[C]) extends Condition[C]

sealed trait RuleT[C] extends Expr[C, Option[Location]] {
  def evaluate(context: C): RuleResult = this match {
    case Rule(condition, location) =>
      for {
        c <- condition.evaluate(context)
      } yield c.mapBoth { case (auditInfo, result) =>
        val l = Option(result).collect { case true => location }
        (auditInfo, l)
      }
  }

  def withName(name: String): RuleT[C] = this match {
    case Rule(condition, location) => RuleWithName(condition, location, name)
    case r@RuleWithName(_, _, _) => r
  }
}

case class When[C](condition: Condition[C]) {
  def thenReturn(location: Location): RuleT[C] = Rule(condition, location)
}

case class Rule[C](condition: Condition[C], location: Location) extends RuleT[C]
case class RuleWithName[C](condition: Condition[C], location: Location, name: String) extends RuleT[C] with Name

sealed trait Name { self: RuleT[_] =>
  def name: String
}

object Test extends App {
  import model.Conditions._
  assert(When(And(ggEnrolmentsAvailable, Not(loggedInViaVerify))) == When(ggEnrolmentsAvailable and Not(loggedInViaVerify)))
}
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

import cats.data.Writer
import model.Location
import model.RoutingReason.RoutingReason
import scala.concurrent.ExecutionContext.Implicits.global

import scala.concurrent.Future

sealed trait Expr[Context, Result] {
  def evaluate(context: Context): Future[Writer[AuditInfo, Result]]
}

case class Pure[C](f: C => Future[Boolean], routingReason: RoutingReason) extends Condition[C] with Reason
case class And[C](c1: Condition[C], c2: Condition[C]) extends Condition[C]
case class Or[C](c1: Condition[C], c2: Condition[C]) extends Condition[C]
case class Not[C](condition: Condition[C]) extends Condition[C]
case class When[C](condition: Condition[C])

sealed trait Condition[C] extends Expr[C, Boolean] {

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
}

object ConditionT {
  implicit class ConditionOps[C](condition: Condition[C]) {
    def and(other: Condition[C]) = And(condition, other)
    def or(other: Condition[C]) = Or(condition, other)
  }

  def not[C](condition: Condition[C]) = Not(condition)
}

sealed trait Reason { self: Condition[_] =>
  def routingReason: RoutingReason
}

sealed trait Rule[C] extends Expr[C, Option[Location]] {

  def evaluate(context: C): RuleResult = {

    def go(condition: Condition[C], location: Location, name: Option[String] = None): RuleResult = for {
      c <- condition.evaluate(context)
    } yield c.mapBoth { case (auditInfo, result) =>
      val l = Option(result).collect { case true => location }
      val info = name.fold(auditInfo)(n => auditInfo.copy(ruleApplied = Some(n)))
      (info, l)
    }

    this match {
      case BaseRule(condition, location) => go(condition, location)
      case RuleWithName(condition, location, name) => go(condition, location, Option(name))
    }
  }
}

object WhenT {
  implicit class WhenOps[C](when: When[C]) {
    def thenReturn(location: Location): Rule[C] = when match {
      case When(condition) => BaseRule(condition, location)
    }
  }
}

object RuleT {
  implicit class RuleOps[C](rule: Rule[C]) {
    def withName(name: String): Rule[C] = rule match {
      case BaseRule(condition, location) => RuleWithName(condition, location, name)
      case r@RuleWithName(_, _, _) => r.copy(name = name)
    }
  }

  def when[C](condition: Condition[C]) = When(condition)
}

private case class BaseRule[C](condition: Condition[C], location: Location) extends Rule[C]
private case class RuleWithName[C](condition: Condition[C], location: Location, name: String) extends Rule[C] with Name

sealed trait Name { self: Rule[_] =>
  def name: String
}
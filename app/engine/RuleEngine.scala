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
import model.{Location, _}

import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.Future

trait RuleEngine {

  def rules: List[Rule[RuleContext]]

  def defaultLocation: Location

  def defaultRuleName: String

  def matchRulesForLocation(ruleContext: RuleContext): Future[Writer[AuditInfo, Location]] = {
    rules.foldLeft(emptyRuleResult) { (result, rule) =>
      result.flatMap {
        case r0 if r0.value.isDefined => Future.successful(r0)
        case _ => rule.evaluate(ruleContext)
      }
    } map { result =>
      result mapBoth ((auditInfo, location) => location match {
        case Some(l) => (auditInfo, l)
        case _ => (auditInfo.copy(ruleApplied = Some(defaultRuleName)), defaultLocation)
      })
    }
  }
}

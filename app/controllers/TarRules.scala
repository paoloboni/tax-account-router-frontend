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

package controllers

import engine.{Rule, RuleEngine}
import model.Conditions._
import model.Locations._
import model.RuleContext

object TarRules extends RuleEngine {

  import engine.Condition._
  import engine.Rule._
  import engine.When._

  override val defaultLocation = BusinessTaxAccount

  override val defaultRuleName = "bta-home-page-passed-through"

  override val rules: List[Rule[RuleContext]] = List(

    rule("pta-home-page-for-verify-user") ~> (when(loggedInViaVerify) thenReturn PersonalTaxAccount),

    rule("bta-home-page-gg-unavailable") ~> (when(isAGovernmentGatewayUser and not(ggEnrolmentsAvailable)) thenReturn BusinessTaxAccount),

    rule("bta-home-page-for-user-with-business-enrolments") ~> (when(isAGovernmentGatewayUser and hasAnyBusinessEnrolment) thenReturn BusinessTaxAccount),

    rule("bta-home-page-sa-unavailable") ~> (when(isAGovernmentGatewayUser and hasSaEnrolments and not(saReturnAvailable)) thenReturn BusinessTaxAccount),

    rule("bta-home-page-for-user-with-no-previous-return") ~> (when(isAGovernmentGatewayUser and hasSaEnrolments and not(hasPreviousReturns)) thenReturn BusinessTaxAccount),

    rule("bta-home-page-for-user-with-partnership-or-self-employment") ~> (when(isAGovernmentGatewayUser and hasSaEnrolments and (isInAPartnership or isSelfEmployed)) thenReturn BusinessTaxAccount),

    rule("bta-home-page-for-user-with-no-partnership-and-no-self-employment-and-no-nino") ~> (when(isAGovernmentGatewayUser and hasSaEnrolments and not(isInAPartnership) and not(isSelfEmployed) and not(hasNino)) thenReturn BusinessTaxAccount),

    rule("pta-home-page-for-user-with-no-partnership-and-no-self-employment") ~> (when(isAGovernmentGatewayUser and hasSaEnrolments and not(isInAPartnership) and not(isSelfEmployed)) thenReturn PersonalTaxAccount),

    rule("bta-home-page-affinity-group-unavailable") ~> (when(not(hasAnyInactiveEnrolment) and not(affinityGroupAvailable)) thenReturn BusinessTaxAccount),

    rule("pta-home-page-individual") ~> (when(not(hasAnyInactiveEnrolment) and hasIndividualAffinityGroup) thenReturn PersonalTaxAccount)
  )
}

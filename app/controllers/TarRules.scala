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

import engine.{Not, RuleEngine, When}
import model.Locations._
import model.Conditions._
import model._

object TarRules extends RuleEngine {

  override val defaultLocation = BusinessTaxAccount

  override val rules = List(
    When(loggedInViaVerify) thenReturn PersonalTaxAccount withName "pta-home-page-for-verify-user",

    When(isAGovernmentGatewayUser and Not(ggEnrolmentsAvailable)) thenReturn BusinessTaxAccount withName "bta-home-page-gg-unavailable",

    When(IsAGovernmentGatewayUser And HasAnyBusinessEnrolment) thenReturn BusinessTaxAccount withName "bta-home-page-for-user-with-business-enrolments",

    When(IsAGovernmentGatewayUser And HasSaEnrolments And Not(SAReturnAvailable)) thenReturn BusinessTaxAccount withName "bta-home-page-sa-unavailable",

    When(IsAGovernmentGatewayUser And HasSaEnrolments And Not(HasPreviousReturns)) thenReturn BusinessTaxAccount withName "bta-home-page-for-user-with-no-previous-return",

    When(IsAGovernmentGatewayUser And HasSaEnrolments And (IsInAPartnership or IsSelfEmployed)) thenReturn BusinessTaxAccount withName "bta-home-page-for-user-with-partnership-or-self-employment",

    When(IsAGovernmentGatewayUser And HasSaEnrolments And Not(IsInAPartnership) And Not(IsSelfEmployed) And Not(HasNino)) thenReturn BusinessTaxAccount withName "bta-home-page-for-user-with-no-partnership-and-no-self-employment-and-no-nino",

    When(IsAGovernmentGatewayUser And HasSaEnrolments And Not(IsInAPartnership) And Not(IsSelfEmployed)) thenReturn PersonalTaxAccount withName "pta-home-page-for-user-with-no-partnership-and-no-self-employment",

    When(Not(HasAnyInactiveEnrolment) And Not(AffinityGroupAvailable)) thenReturn BusinessTaxAccount withName "bta-home-page-affinity-group-unavailable",

    When(Not(HasAnyInactiveEnrolment) And HasIndividualAffinityGroup) thenReturn PersonalTaxAccount withName "pta-home-page-individual",

    When(AnyOtherRuleApplied) thenReturn BusinessTaxAccount withName "bta-home-page-passed-through"
  )
}

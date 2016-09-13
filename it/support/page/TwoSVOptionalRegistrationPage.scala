/*
 * Copyright 2015 HM Revenue & Customs
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

package support.page

import java.net.URLEncoder

import com.github.tomakehurst.wiremock.client.WireMock._
import support.Env
import support.stubs.{Stub, StubbedPage}

object TwoSVOptionalRegistrationStubPage extends Stub with StubbedPage {
  override def create() = stubOut(urlEqualTo(TwoSVOptionalRegistrationPage.uri), "2SV Optional Registration Page")
}

object TwoSVOptionalRegistrationPage extends WebPage {
  private val hostPort = s"http://${Env.stubHost}:${Env.stubPort}"
  private val continueUrl = URLEncoder.encode(s"$hostPort/business-account", "UTF-8")
  private val failureUrl  = URLEncoder.encode(s"$hostPort/business-account", "UTF-8")
  private val queryString = s"origin=business-tax-account&failure=$failureUrl&continue=$continueUrl"

  val uri = s"/coafe/two-step-verification/register?$queryString"
  override val url = s"$hostPort$uri"

  override def assertPageLoaded() = assertPageIs("2SV Optional Registration Page")
}
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

package connector

import config.WSHttp
import play.api.libs.json._
import uk.gov.hmrc.play.config.ServicesConfig
import uk.gov.hmrc.play.http.{HeaderCarrier, HttpGet, HttpPost}

object AffinityGroupValue {
  val INDIVIDUAL = "Individual"
  val ORGANISATION = "Organisation"
  val AGENT = "Agent"
}

trait UserDetailsConnector {

  val serviceUrl: String

  def http: HttpGet with HttpPost

  def getUserDetails(userDetailsUri: String)(implicit hc: HeaderCarrier) = http.GET[UserDetails](userDetailsUri)
}

case class CredentialRole(val value: String) extends AnyVal {
  def isAdmin = value == "User"
}

object UserDetailsConnector extends UserDetailsConnector with ServicesConfig {
  override lazy val serviceUrl = baseUrl("user-details")
  override lazy val http = WSHttp
}

case class UserDetails(credentialRole: Option[CredentialRole], affinityGroup: String) {
  def isAdmin = credentialRole.fold(false)(_.isAdmin)
}

object UserDetails {
  implicit val credentialRoleReads: Reads[CredentialRole] = new Reads[CredentialRole] {
    override def reads(json: JsValue): JsResult[CredentialRole] = JsSuccess(CredentialRole(json.as[String]))
  }
  implicit val reads: Reads[UserDetails] = Json.reads[UserDetails]
}

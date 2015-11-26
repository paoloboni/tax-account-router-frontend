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

package services

import model.Location._
import org.joda.time.DateTime
import play.api.Configuration
import repositories.{HourlyLimitId, HourlyLimitsCacheRepository}

import scala.concurrent.{ExecutionContext, Future}

trait HourlyLimitService {

  def hourlyLimitsCacheRepository: HourlyLimitsCacheRepository

  def applyHourlyLimit(location: LocationType, fallbackLocation: LocationType, userId: String, configurationForLocation: Configuration)(implicit ec: ExecutionContext): Future[LocationType] = {
    val hourOfDay = DateTime.now().getHourOfDay
    val hourlyLimit = configurationForLocation.getInt(s"hourlyLimit.$hourOfDay").fold(configurationForLocation.getInt("hourlyLimit.other")) {
      Some(_)
    }

    hourlyLimit match {
      case Some(limit) =>
        val id = HourlyLimitId(location, hourOfDay)
        val updateResult = hourlyLimitsCacheRepository.createOrUpdate(id, limit, userId)

        updateResult.flatMap {
          case resultOption =>
            resultOption.map { databaseUpdate =>
              Future(location)
            }.getOrElse {
              val hourlyLimitAlreadyExists = hourlyLimitsCacheRepository.exists(id, userId)
              hourlyLimitAlreadyExists.map {
                case true => location
                case false => fallbackLocation
              }
            }
        }
      case None => Future(location)
    }
  }
}

object HourlyLimitService extends HourlyLimitService {
  override def hourlyLimitsCacheRepository: HourlyLimitsCacheRepository = HourlyLimitsCacheRepository()
}

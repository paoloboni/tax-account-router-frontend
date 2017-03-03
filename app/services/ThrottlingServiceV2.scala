package services

import engine.{AuditInfo, EngineResult, ThrottlingInfo}
import model.Locations.{BusinessTaxAccount, PersonalTaxAccount}
import model.{Location, Locations, RuleContext}
import org.joda.time.DateTime
import play.api.mvc.{AnyContent, Request}
import play.api.{Configuration, Play}
import repositories.RoutingCacheRepository
import play.api.Play.current
import scala.concurrent.ExecutionContext.Implicits.global

import scala.util.Random

trait Throttling {

  def random: Random

  def routingCacheRepository: RoutingCacheRepository

  def hourlyLimitService: HourlyLimitService

  def configuration: Configuration

  val throttlingEnabled = Play.configuration.getBoolean("throttling.enabled").getOrElse(false)
  val stickyRoutingEnabled = Play.configuration.getBoolean("sticky-routing.enabled").getOrElse(false)

  val longLiveCacheExpirationTime = Play.configuration.getString("sticky-routing.long-live-cache-expiration-time")
  val shortLiveCacheDuration = Play.configuration.getInt("sticky-routing.short-live-cache-duration")

  val documentExpirationTime = Map(
    (PersonalTaxAccount, PersonalTaxAccount) -> longLiveCacheExpirationTime.map(t => Instant(DateTime.parse(t))),
    (BusinessTaxAccount, BusinessTaxAccount) -> shortLiveCacheDuration.map(t => Duration(t)),
    (PersonalTaxAccount, BusinessTaxAccount) -> shortLiveCacheDuration.map(t => Duration(t))
  )

  def throttle(currentResult: EngineResult, ruleContext: RuleContext): EngineResult = {

    def configurationForLocation(location: Location, request: Request[AnyContent]): Configuration = {

      def getLocationSuffix(location: Location, request: Request[AnyContent]): String = {
        location match {
          case PersonalTaxAccount =>
            if (request.session.data.contains("token")) "-gg"
            else "-verify"
          case _ => ""
        }
      }

      val suffix = getLocationSuffix(location, request)
      configuration.getConfig(s"throttling.locations.${location.name}$suffix").getOrElse(Configuration.empty)
    }

    import cats.instances.all._
    import cats.syntax.flatMap._
    import cats.syntax.functor._

    def throttlePercentage: (Configuration) => Int = configurationForLocation => {
      configurationForLocation.getString("percentageBeToThrottled").map(_.toInt).getOrElse(0)
    }

    def hourlyLimit(hourOfDay: Int): (Configuration) => Option[Int] = configurationForLocation => {
      configurationForLocation.getInt(s"hourlyLimit.$hourOfDay").orElse(configurationForLocation.getInt("hourlyLimit.other"))
    }

    def findFallbackFor(location: Location): (Configuration) => Location = configurationForLocation => {
      val fallback = for {
        fallbackName <- configurationForLocation.getString("fallback")
        fallback <- Locations.find(fallbackName)
      } yield fallback
      fallback.getOrElse(location)
    }

    for {
      result <- currentResult
      userIdentifier <- ruleContext.internalUserIdentifier
    } yield {
      userIdentifier match {
        case None => result
        case Some(userId) =>
          result mapBoth {
            case (auditInfo, location) if throttlingEnabled =>

              val throttle: (Configuration) => (AuditInfo, Location) =
                for {
                  percentage <- throttlePercentage
                  throttleDestination <- findFallbackFor(location)
                  hourOfDay = DateTime.now().getHourOfDay
                  hourlyLimit <- hourlyLimit(hourOfDay)
                  randomNumber = random.nextInt(100) + 1
                } yield {
                  val throttlingInfo = ThrottlingInfo(throttlingPercentage = Some(percentage), location != throttleDestination, location, throttlingEnabled, stickyRoutingApplied = false)
                  if (randomNumber <= percentage) {
                    (auditInfo.copy(throttlingInfo = Some(throttlingInfo)), throttleDestination)
                  } else {
                    hourlyLimitService.applyHourlyLimit(location, throttleDestination, userId, hourlyLimit, hourOfDay)
                    (auditInfo.copy(throttlingInfo = Some(throttlingInfo)), throttleDestination)
                  }

                }
              throttle(configurationForLocation(location, ruleContext.request_))

            case (auditInfo, location) =>
              val throttlingInfo = ThrottlingInfo(throttlingPercentage = None, throttled = false, location, throttlingEnabled, stickyRoutingApplied = false)
              (auditInfo.copy(throttlingInfo = Some(throttlingInfo)), location)
          }
      }
    }
  }
}

object Throttling extends Throttling {

  override lazy val random: Random = Random

  override lazy val routingCacheRepository: RoutingCacheRepository = RoutingCacheRepository()

  override lazy val hourlyLimitService: HourlyLimitService = HourlyLimitService

  override val configuration: Configuration = Play.configuration
}
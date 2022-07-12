import java.util.UUID
import scala.util.{Failure, Success, Try}

/**
 * A Load Balancer provider.
 */
trait Provider {
  /**
   * Retrieve the unique ID of the provider.
   *
   * @return The unique ID of the provider.
   */
  def get: Try[String]

  /**
   * Whether this provider is alive or not.
   *
   * @return True when it is alive, false if not.
   */
  def isAlive: Boolean

  /**
   * Kills the provider.
   *
   * @return A new provider which is dead.
   */
  def die: Provider

  /**
   * Relives the provider
   *
   * @return A new provider which is alive.
   */
  def relive: Provider
}

case class AliveProvider(id: String = UUID.randomUUID().toString) extends Provider {
  def get: Success[String] = Success(id)

  def isAlive = true

  def die = DeadProvider(id)

  def relive = this
}

case class DeadProvider(id: String = UUID.randomUUID().toString) extends Provider {
  def get: Failure[String] = Failure(LBException.noProvider)

  def isAlive = false

  def die = this

  def relive = AliveProvider(id)
}


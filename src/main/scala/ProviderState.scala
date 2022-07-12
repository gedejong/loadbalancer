/**
 * The state of an associated provider.
 *
 * @param validChecksNeeded The number of checks needed to validate whether the provider is back online.
 */
case class ProviderState(validChecksNeeded: Int = 0) {
  def isActive: Boolean = validChecksNeeded == 0

  def inactivate: ProviderState = copy(validChecksNeeded = 2)

  def activate: ProviderState = copy(validChecksNeeded = 0)

  def validCheck: ProviderState = copy(validChecksNeeded = Math.max(0, validChecksNeeded - 1))
}

object ProviderState {
  val initial: ProviderState = ProviderState()
}
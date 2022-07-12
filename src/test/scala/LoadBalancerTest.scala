import org.scalatest._
import flatspec._
import matchers._
import LoadBalancer._

class LoadBalancerTest extends AnyFlatSpec with should.Matchers with TryValues {

  "A LBState" should "be properly initialised" in {
    val count = 10
    val program = for {
      _ <- providers(count)
      firstRoundRobin <- getMultiRoundRobin(count) // Gets all providers registered
      secondRoundRobin <- getMultiRoundRobin(count) // Gets them again in same order
    } yield (firstRoundRobin, secondRoundRobin)

    val result = program.runA(LoadBalancer())
    // We should return all registered providers
    result._1.success.value should have length (count)
    // We should see the same providers when calling all of the providers in the round robin twice
    result._1.success.value should be (result._2.success.value)
  }

  "A LBState" should "not provide a dead provider" in {
    val provider1 = AliveProvider("alive")
    val provider2 = DeadProvider("dead")

    val program = for {
      _ <- register(provider1)
      _ <- register(provider2)
      _ <- check
      providers <- getMultiRoundRobin(2)
    } yield providers
    // When running with a dead and an alive provider, only provide the alive one
    val providersTry = program.runA(LoadBalancer())
    providersTry.success.value should have length (2)
    providersTry.success.value should not contain (provider2)
    providersTry.success.value should contain (provider1)
  }

  "A LBState" should "return failure when there is no available provider" in {
    var program = for {
      _ <- register(DeadProvider("dead"))
      _ <- check
      provider <- getRoundRobin
    } yield provider
    val providerTry = program.runA(LoadBalancer())
    providerTry.failure.exception should be (LBException.noProvider)
  }

  "A LBState" should "return a provider which becomes alive again after two checks" in {
    val provider = DeadProvider("p1")
    val program = for {
      _ <- register(provider)
      _ <- check
      providerTry1 <- getRoundRobin // Should return Failure
      _ <- relive(provider)
      _ <- check
      providerTry2 <- getRoundRobin // Should still return Failure
      _ <- check
      providerTry3 <- getRoundRobin // Now the provider should be back in active mode
    } yield (providerTry1, providerTry2, providerTry3)
    val (providerTry1, providerTry2, providerTry3) = program.runA(LoadBalancer())
    providerTry1.failure.exception should be (LBException.noProvider)
    providerTry2.failure.exception should be (LBException.noProvider)
    providerTry3.success.value should be (AliveProvider("p1"))
  }
}

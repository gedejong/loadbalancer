import scala.util.{Failure, Success, Try}

/**
 * Represents the internal state of Load Balancer.
 * @param providers The registered providers and the provider state.
 * @param seed A random seed used to select random providers.
 */
case class LoadBalancer(providers: Seq[(ProviderState, Provider)] = Seq(), seed: Seed = Seed(0L)) {
  def register(provider: Provider) =
    LoadBalancer(
      providers = providers :+ (ProviderState.initial, provider),
      seed = seed)

  def activeProviders: Seq[Provider] = providers.map(_._2).filter(_.isAlive)
}

/**
 * State methods for the Load Balancer.
 */
object LoadBalancer {

  /**
   * Registers the provider in the state.
   * @param provider The provider to register.
   * @return A State with the given provider as active.
   */
  def register(provider: Provider): State[LoadBalancer, Unit] = State { s => (s.register(provider), ()) }

  /**
   * For testing purposed, kills the given provider but this is not checked yet).
   * @param provider The provider to kill
   * @return The modified state.
   */
  def die(provider: Provider): State[LoadBalancer, Unit] = State.modify { lbState: LoadBalancer =>
    lbState.copy(providers = lbState.providers.map {
      case (s, p) if p == provider => (s, p.die)
      case (s, p) => (s, p)
    })
  }

  /**
   * For testing purposed, brings the given provider back alive (but this is not checked yet).
   * @param provider The provider to bring back alive.
   * @return The modified state.
   */
  def relive(provider: Provider): State[LoadBalancer, Unit] = State.modify { lbState: LoadBalancer =>
    lbState.copy(providers = lbState.providers.map {
      case (s, p) if p == provider => (s, p.relive)
      case (s, p) => (s, p)
    })
  }

  /**
   * Retrieves a random load balancer provider using the internal Seed state.
   */
  val getRandom: State[LoadBalancer, Try[Provider]] = {
    State { lbState =>
      val lengthActive = lbState.activeProviders.length
      if (lengthActive == 0) {
        (lbState, Failure(LBException.noProvider))
      } else {
        val (seed, l) = Seed.between(lbState.seed)(0, lbState.activeProviders.length)
        val provider = lbState.activeProviders(l.toInt)
        (lbState.copy(seed = seed), Success(provider))
      }
    }
  }

  /**
   * Retrieves a load balancer provider using round robin. It changes the state so
   * the given provider will be last returned when this state is called again.
   */
  val getRoundRobin: State[LoadBalancer, Try[Provider]] = {
    State {
      case s@LoadBalancer(providers, randomState) =>
        val index = providers.indexWhere(_._1.isActive)
        if (index < 0) (s, Failure(LBException.noProvider))
        else {
          val provider = providers(index)
          val newProviders = providers.filterNot(_ == provider) :+ provider
          (LoadBalancer(newProviders, randomState), Success(provider._2))
        }
    }
  }

  /**
   * Exclude the provider from the active set.
   * @param provider The provider to remove from the active set.
   * @return A state which has the provider removed from ihe active set.
   */
  def exclude(provider: Provider): State[LoadBalancer, Unit] =
    State.modify {
      case LoadBalancer(providers, randomState) =>

        val newProviders = providers.map {
          case (ps, p) => if (p == provider) (ps.inactivate, p) else (ps, p)
        }

        LoadBalancer(newProviders, randomState)
    }

  /**
   * Include the provider in the active set.
   * @param provider The provider to add to the active set.
   * @return A state which has the provider in the active set.
   */
  def include(provider: Provider): State[LoadBalancer, Unit] =
    State.modify {
      case LoadBalancer(providers, randomState) =>

        val newProviders = providers.map {
          case (ps, p) => if (p == provider) (ps.activate, p) else (ps, p)
        }

        LoadBalancer(newProviders, randomState)
    }

  /**
   * Checks the active providers to see if they are still alive and the inactive
   * providers to see if they are back alive and changes the state correctly.
   */
  val check: State[LoadBalancer, Unit] =
    State.modify {
      case LoadBalancer(providers, randomState) =>
        val newProviders = providers.map {
          case (ps, p) if ps.isActive && !p.isAlive => (ps.inactivate, p)
          case (ps, p) if !ps.isActive && p.isAlive => (ps.validCheck, p)
          case (ps, p) => (ps, p)
        }

        LoadBalancer(newProviders, randomState)
    }

  /**
   * Creates and registers a number of providers with the name 'lbpi-<provider-id>'
   *
   * @param count The number of providers to create
   * @return A State after which the load balancer has the providers registered
   */
  def providers(count: Long = 10): State[LoadBalancer, Unit] =
    if (count <= 0) State.unit(())
    else
      for {
        _ <- register(AliveProvider(s"lbpi-$count"))
        _ <- providers(count - 1)
      } yield ()

  /**
   * Retrieves a sequence of `count` providers from the load balancer using the round-robin algorithm.
   * When no providers are available, the result from the state will be failure.
   *
   * @param count The number of providers to retrieve
   * @return A State which retrieves the number of providers.
   */
  def getMultiRoundRobin(count: Long = 10): State[LoadBalancer, Try[Seq[Provider]]] =
    if (count <= 0) State.unit(Success(Seq[Provider]()))
    else
      for {
        providerTry <- getRoundRobin
        otherProvidersTry <- getMultiRoundRobin(count - 1)
      } yield for {
        provider <- providerTry
        otherProviders <- otherProvidersTry
      } yield otherProviders :+ provider
}

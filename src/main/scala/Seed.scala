final case class Seed(long: Long) {
  def next = Seed(long * 6364136223846793005L + 1442695040888963407L)
}

object Seed {
  def nextLong(seed: Seed): (Seed, Long) =
    (seed.next, seed.long)

  def between(seed: Seed)(from: Long, to: Long): (Seed, Long) = {
    val (newSeed, l) = nextLong(seed)
    (newSeed, (l % (to - from)) + from)
  }
}
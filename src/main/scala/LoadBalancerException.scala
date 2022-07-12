object LBException {
  val noProvider = new LBException("No provider available")
  val deadProvider = new LBException("Provider is dead")
}

class LBException(msg: String) extends Exception(msg)
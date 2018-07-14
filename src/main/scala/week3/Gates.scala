package week3

abstract class Gates extends Simulation {
  def InverterDelay: Int
  def AndDelay: Int
  def OrDelay: Int

  class Wire {
    private var sigVal = false
    private var actions: List[Action] = Nil
    def getSignal: Boolean = sigVal
    def setSignal(sig: Boolean): Unit =
      if (sig != sigVal) {
        sigVal = sig
        actions foreach (_())
      }

    def addAction(a: Action): Unit = {
      actions = a :: actions
      a()
    }
  }



  def inverter(input: Wire, output: Wire): Unit = {
    def invertAction(): Unit = {
      val inputSig = input.getSignal
      afterDelay(InverterDelay) {output setSignal !inputSig}
    }
    input addAction invertAction
  }

  def andGate(input1: Wire, input2: Wire, output: Wire): Unit = {
    def andAction(): Unit = {
      val input1Sig = input1.getSignal
      val input2Sig = input2.getSignal
      afterDelay(AndDelay) {output setSignal (input1Sig & input2Sig)}
    }
    input1 addAction andAction
    input2 addAction andAction
  }

  def orGate(input1: Wire, input2: Wire, output: Wire): Unit = {
    def orAction(): Unit = {
      val input1Sig = input1.getSignal
      val input2Sig = input2.getSignal
      afterDelay(OrDelay) {output setSignal (input1Sig | input2Sig)}
    }
    input1 addAction orAction
    input2 addAction orAction
  }

  def probe(name: String, wire: Wire): Unit = {
    def probeAction(): Unit = {
      println(s"$name $currentTime value = ${wire.getSignal}")
    }
    wire.addAction(probeAction)
  }
}

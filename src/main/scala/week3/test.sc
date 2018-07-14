import week3._

object sim extends Circuits with Parameters
import sim._

val in1, in2, sum, carry = new Wire
halfAdder(in1, in2, sum, carry)
probe("sum", sum)
probe("carry", carry)

in1.setSignal(true)
run()
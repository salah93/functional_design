package calculator

object Polynomial {
  def computeDelta(a: Signal[Double], b: Signal[Double],
      c: Signal[Double]): Signal[Double] = {
    Signal({
      val b2 = math.pow(b(), 2)
      val aC = a() * c()
      b2 - (4 * aC)
    })
  }

  def computeSolutions(a: Signal[Double], b: Signal[Double],
      c: Signal[Double], delta: Signal[Double]): Signal[Set[Double]] = {
    Signal({
      if (delta() < 0) Set()
      else if(a() == 0) Set(-c() / b())
      else {
        Set(
            (-b() + math.sqrt(delta()))  / (2 * a()),
            (-b() - math.sqrt(delta()))  / (2 * a())
        )
      }
    })
  }
}

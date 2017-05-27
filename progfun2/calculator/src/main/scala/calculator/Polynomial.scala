package calculator

object Polynomial {
  def computeDelta(a: Signal[Double], b: Signal[Double],
      c: Signal[Double]): Signal[Double] = {

    Signal((b() * b()) - (4 * a() * c()))
  }

  def computeSolutions(a: Signal[Double], b: Signal[Double],
      c: Signal[Double], delta: Signal[Double]): Signal[Set[Double]] = {

    Signal(if (delta() < 0.0) {
     Set(delta())
    } else {
      val deltaRoot = math.sqrt(delta())
      Set(computeSolution(a, b, deltaRoot), computeSolution(a, b, - deltaRoot))
    })
  }

  private def computeSolution(a: Signal[Double], b: Signal[Double], deltaRoot: Double): Double = {
    (-b() + deltaRoot) / (2 * a())
  }
}

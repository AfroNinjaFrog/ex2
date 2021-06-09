

class Line(points: Array[Point]) {
  private var xs: Array[Double]  = Array[Double]()
  private var ys: Array[Double] = Array[Double]()
  points.foreach(point => {
    xs = xs :+ point.x
    ys = ys :+ point.y
  })
  val a: Double = Util.covariance(xs, ys) / Util.variance(xs)
  val b: Double = Util.average(ys) - (a * Util.average(xs))

  def f(x: Double): Double = {
    this.a * x + this.b
  }

  def dist(point: Point): Double = {
    Math.abs(this.f(point.x) - point.y)
  }

}

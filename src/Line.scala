import scala.collection.mutable.ArrayBuffer

class Line(points: Array[Point]) {
  private val xs = new ArrayBuffer[Double]()
  private val ys = new ArrayBuffer[Double]()
  points.foreach(point => {
    xs += point.x
    ys += point.y
  })
  val a: Double = Util.covariance(Array.from(xs), Array.from(ys)) / Util.variance(Array.from(xs))
  val b: Double = Util.average(Array.from(ys)) - (a * Util.average(Array.from(xs)))

  def f(x: Double): Double = {
    this.a * x + this.b
  }

  def dist(point: Point): Double = {
    Math.abs(this.f(point.x) - point.y)
  }

}

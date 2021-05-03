import scala.collection.mutable.ArrayBuffer

object Util {
  def Max[A](list: List[A], comporer: (A, A) => Int): A = {
    var maxVal: A = list.head
    for (listItem <- list) {
      if (comporer(listItem, maxVal) > 0) {
        maxVal = listItem
      }
    }
    maxVal
  }

  def map[A, B, C](list: List[A], changeAToB: (A) => B, changeBToC: (B) => C): List[C] = {
    list.map(listItem => changeBToC(changeAToB(listItem)))
  }

  def isSorted[A](list: List[A], comparer: (A, A) => Boolean): Boolean = {
    var lastValue: A = list.head
    var firstPass = true
    list.foreach(listItem => {
      if (firstPass) {
        lastValue = listItem
        firstPass = false
      } else if (comparer(lastValue, listItem)) {
        lastValue = listItem
      } else {
        false
      }
    })
    true
  }

  def probs(arr: Array[Double]): Array[Double] = {
    var occurences: Map[String, Int] = Map()
    arr.foreach(arrayItem => {
      occurences += (String.valueOf(arrayItem) -> ((occurences getOrElse(String.valueOf(arrayItem), 0)) + 1))
    })
    arr.map(arrItem => occurences.getOrElse(String.valueOf(arrItem), 0.0).asInstanceOf[Int].toDouble / arr.length)
  }

  def entropyCalc(arr: Array[Double]): Double = {
    -probs(arr).map[Double](arrItem => arrItem * (Math.log(arrItem) / Math.log(2))).sum
  }

  def mu(arr: Array[Double]): Double = {
    probs(arr).zip(arr).map(i => i._1 * i._2).sum
  }

  def variance(arr: Array[Double]): Double = {
    probs(arr).zip(arr).map(i => i._1 * Math.pow(i._2 - mu(arr), 2)).sum
  }

  def zscore(arr: Array[Double], x: Double): Double = {
    (x - mu(arr)) / Math.sqrt(variance(arr))
  }
  def absZScore(vector: Vector[Double], x: Double): Double = {
    Math.abs(zscore(Array.from(vector), x))
  }
  def covariance(xs: Array[Double], ys: Array[Double]): Double = {
    mu(xs.zip(ys).map(i => i._1 * i._2)) - (mu(xs) * mu(ys))
  }

  def pearson(xs: Array[Double], ys: Array[Double]): Double = {
    (covariance(xs, ys)) / (Math.sqrt(variance(xs)) * Math.sqrt(variance(ys)))
  }

  def average(arr: Array[Double]): Double = {
    arr.sum / arr.length
  }

  def getAFromPoints(points: Array[Point]): Double = {
    val xs = new ArrayBuffer[Double]()
    val ys = new ArrayBuffer[Double]()
    points.foreach(point => {
      xs += point.x
      ys += point.y
    })
    Util.covariance(Array.from(xs), Array.from(ys)) / Util.variance(Array.from(xs))
  }

  def getBFromPointsAndA(points: Array[Point]): Double = {
    val a = this.getAFromPoints(points)
    val xs = new ArrayBuffer[Double]()
    val ys = new ArrayBuffer[Double]()
    points.foreach(point => {
      xs += point.x
      ys += point.y
    })
    Util.average(Array.from(ys)) - (a * Util.average(Array.from(xs)))
  }
}
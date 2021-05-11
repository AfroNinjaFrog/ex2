import scala.collection.mutable.ArrayBuffer

object Util {
  def max[A](list: List[A], comporer: (A, A) => Int): A = {
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
        return false
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

  def entropy(arr: Array[Double]): Double = {
    -(getUniqueProbs(arr).map(arrItem => arrItem * (Math.log10(arrItem) / Math.log10(2))).sum)
  }

  def getUniqueProbs(arr: Array[Double]): Array[Double] = {
    probs(arr).zip(arr).zipWithIndex.filter(values => values._2 == arr.indexOf(values._1._2)).map(values => values._1._1)
  }

  def getUniqueProbsAndValues(arr: Array[Double]): Array[(Double, Double)] = {
    probs(arr).zip(arr).zipWithIndex.filter(values => values._2 == arr.indexOf(values._1._2)).map(values => (values._1._1, values._1._2))
  }

  def mu(arr: Array[Double]): Double = {
    getUniqueProbsAndValues(arr).map(i => i._1 * i._2).sum
  }

  def variance(arr: Array[Double]): Double = {
    getUniqueProbsAndValues(arr).map(i => i._1 * Math.pow(i._2 - mu(arr), 2)).sum
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

  def findMaxDistance(line: Line, ps: Array[Point]): Double = {
    ps.map(p => line.dist(p)).max
  }

  def filterPointerPairs(arr: Array[Int]): Array[Int] = {
    var removeArr: Array[Boolean] = Array.fill(arr.length) {
      true
    }
    arr.zipWithIndex.foreach(value => {
      if (removeArr(value._2)) {
        if (arr(arr(value._2)) == value._2) {
          removeArr(arr(value._2)) = false
        }
      }
    })
    val filteredArr = arr.zipWithIndex.filter((value) => removeArr(value._2)).map(value => value._1)
    filteredArr
  }

  def calcCovarianceMatrix(normal: TimeSeries): Array[Array[Double]] = {
    val matrixOfCovariance = Array.ofDim[Double](normal.features.length, normal.features.length)
    val featuresValues: Vector[Vector[Double]] = normal.features.map((feature) => normal.getValues(feature).get)
    featuresValues.zipWithIndex.foreach(featureValue => {
      for (otherFeaturesIndex <- featureValue._2 + 1 until featuresValues.length) {
        matrixOfCovariance(featureValue._2)(otherFeaturesIndex) = Math.abs(Util.pearson(Array.from(normal.getValues(normal.features(featureValue._2)).get), Array.from(normal.getValues(normal.features(otherFeaturesIndex)).get)))
      }
    })
    matrixOfCovariance
  }

  def findMaxCov(normal: TimeSeries): Array[MaxCov] = {
    val matrixOfCovariance = calcCovarianceMatrix(normal)
    val maxCovArray: Array[MaxCov] = (Array.fill[MaxCov](normal.features.length)(MaxCov(0, -1)))
    matrixOfCovariance.zipWithIndex.foreach(i => {
      for (j <- (i._2 + 1) until matrixOfCovariance.length) {
        if (matrixOfCovariance(i._2)(j) > maxCovArray(j).maxCov) {
          maxCovArray(j) = MaxCov(matrixOfCovariance(i._2)(j), i._2)
        }
        if (matrixOfCovariance(i._2)(j) > maxCovArray(i._2).maxCov) {
          maxCovArray(i._2) = MaxCov(matrixOfCovariance(i._2)(j), j)
        }
      }
    })
    maxCovArray
  }

  def dist(p1: Point, p2: Point): Double = {
    Math.sqrt(Math.pow(p1.x - p2.x, 2) + Math.pow(p1.y - p2.y, 2))
  }

  def sqrSum(points: Array[Point], pIndex: Integer): Double = {
    points.zipWithIndex.filter(point => point._2 != pIndex).map(point => dist(point._1, points(pIndex))).sum
  }

  def createPointsFromCovariance(maxCovValues: Array[MaxCov], ts: TimeSeries): Vector[Vector[Point]] = {
    Vector.from(maxCovValues.zipWithIndex.map(covValue => (ts.getValues(ts.features(covValue._2)).get
      .zip(ts.getValues(ts.features(covValue._1.indexOfMaxCov)).get)
      .map(twoFeatures => new Point(twoFeatures._1, twoFeatures._2)))))
  }

  def orderByLetterOrder(ts: TimeSeries, val1: Int, val2: Int): String = {
    (if (val1 > val2) (ts.features(val2) + "," + ts.features(val1)) else (ts.features(val1) + "," + ts.features(val2)))
  }
}

case class MaxCov(maxCov: Double, indexOfMaxCov: Int) {}
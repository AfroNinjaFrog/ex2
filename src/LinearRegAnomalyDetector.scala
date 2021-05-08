import scala.collection.mutable

object LinearRegAnomalyDetector extends AnomalyDetector {
  var learntMap: Map[String, String] = Map()
  var learntTimeSeries: TimeSeries = null
  private var learntData: Vector[(Line, Double)] = Vector()
  private var pairs: Array[Int] = Array()

  override def learn(normal: TimeSeries): Map[String, String] = {
    val maxCovValues: Array[MaxCov] = findMaxCov(normal)
    pairs = maxCovValues.map(value => value.indexOfMaxCov)

    var pointsOfCovariancePairs: Vector[Vector[Point]] = Vector()
    pointsOfCovariancePairs = Vector.from(maxCovValues.zipWithIndex.map(covValue => (normal.getValues(normal.features(covValue._2)).get
      .zip(normal.getValues(normal.features(covValue._1.indexOfMaxCov)).get)
      .map(twoFeatures => new Point(twoFeatures._1, twoFeatures._2)))))
    var linesVector: Vector[Line] = pointsOfCovariancePairs.map(pair => new Line(Array.from(pair)))
    learntData = linesVector.zip(pointsOfCovariancePairs).map(lineAndPoints => (lineAndPoints._1, Util.findMaxDistance(lineAndPoints._1, Array.from(lineAndPoints._2))))
    learntMap = learntData.zipWithIndex.map { case (data, index) => (normal.features(index), data._1.toString() + "," + data._2.toString) }.toMap
    learntMap
  }

  override def detect(model: Map[String, String], test: TimeSeries): Vector[(String, Int)] = {
    var anomalies: Vector[(String, Int)] = Vector()
    pairs.zipWithIndex.zip(learntData).foreach((pairAndData) => {
      test.getValues(test.features(pairAndData._1._1)).get.zip(
        test.getValues(test.features(pairAndData._1._2)).get).map(xAndYValues => new Point(xAndYValues._2, xAndYValues._1)).zipWithIndex
        .foreach(point => {
          if (pairAndData._2._1.dist(point._1) > pairAndData._2._2) {
            anomalies = anomalies :+ ((if (pairAndData._1._1 > pairAndData._1._2) (test.features(pairAndData._1._2) + "," + test.features(pairAndData._1._1)) else (test.features(pairAndData._1._1) + "," + test.features(pairAndData._1._2))), point._2)
          }
        })
    })
    anomalies
  }

  def calcCovarianceMatrix(normal: TimeSeries): Array[Array[Double]] = {
    var matrixOfCovariance = Array.ofDim[Double](normal.features.length, normal.features.length)
    var featuresValues: Vector[Vector[Double]] = normal.features.map((feature) => normal.getValues(feature).get)
    featuresValues.zipWithIndex.foreach(featureValue => {
      for (otherFeaturesIndex <- featureValue._2 + 1 until featuresValues.length) {
        matrixOfCovariance(featureValue._2)(otherFeaturesIndex) = Math.abs(Util.pearson(Array.from(normal.getValues(normal.features(featureValue._2)).get), Array.from(normal.getValues(normal.features(otherFeaturesIndex)).get)))
      }
    })
    matrixOfCovariance
  }

  def findMaxCov(normal: TimeSeries): Array[MaxCov] = {
    val matrixOfCovariance = calcCovarianceMatrix(normal)
    var maxCovArray: Array[MaxCov] = (Array.fill[MaxCov](normal.features.length)(MaxCov(0, -1)))
    matrixOfCovariance.zipWithIndex.foreach(i => {
      for (j <- (i._2 + 1) until matrixOfCovariance.length) {
        if (Math.abs(matrixOfCovariance(i._2)(j)) >= 0.9) {
          if (matrixOfCovariance(i._2)(j) > maxCovArray(j).maxCov) {
            maxCovArray(j) = MaxCov(matrixOfCovariance(i._2)(j), i._2)
          }
          if (matrixOfCovariance(i._2)(j) > maxCovArray(i._2).maxCov) {
            maxCovArray(i._2) = MaxCov(matrixOfCovariance(i._2)(j), j)
          }
        }
      }
    })
    maxCovArray
  }

}

case class MaxCov(maxCov: Double, indexOfMaxCov: Int) {}



object SumSqrAnomalyDetector extends AnomalyDetector {
  var learntTimeSeries: TimeSeries = null
  private var learntData: Vector[Double] = Vector()
  private var pairs: Array[Int] = Array()

  override def learn(normal: TimeSeries): Map[String, String] = {
    val maxCovValues: Array[MaxCov] = Util.findMaxCov(normal)
    pairs = maxCovValues.map(value => value.indexOfMaxCov)
    val pointsOfCovariancePairs = Util.createPointsFromCovariance(maxCovValues, normal)

    learntData = pointsOfCovariancePairs.map(vecPoints => vecPoints.zipWithIndex.map(point => Util.sqrSum(Array.from(vecPoints), point._2)).max)
    learntData.zipWithIndex.map{case(data, index) => normal.features(index) -> data.toString}.toMap
  }

  override def detect(model: Map[String, String], test: TimeSeries): Vector[(String, Int)] = {
    var anomalies: Vector[(String, Int)] = Vector()
    pairs.zipWithIndex.zip(learntData).foreach((pairAndData) => {
      var pairPoints:Vector[Point] = test.getValues(test.features(pairAndData._1._1)).get.zip(
        test.getValues(test.features(pairAndData._1._2)).get).map(xAndYValues => new Point(xAndYValues._2, xAndYValues._1))
        pairPoints.zipWithIndex.foreach(point => {
          var maxDistOfPoint = Util.sqrSum(Array.from(pairPoints), point._2)
          if(maxDistOfPoint > pairAndData._2) anomalies = anomalies :+ (Util.orderByLetterOrder(test, pairAndData._1._2, pairAndData._1._1), point._2)
        })
    })
    anomalies.distinct
  }
}

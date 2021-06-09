

object SumSqrAnomalyDetector extends AnomalyDetector {
  private var learntData: Vector[SumSquarePairData] = Vector()
  private var pairs: Array[Int] = Array()

  override def learn(normal: TimeSeries): Map[String, String] = {
    val maxCovValues: Array[MaxCov] = Util.findMaxCov(normal)
    pairs = maxCovValues.map(value => value.indexOfMaxCov)
    val pointsOfCovariancePairs = Util.createPointsFromCovariance(maxCovValues, normal)

    learntData = pointsOfCovariancePairs.map(vecPoints => learnSumSquarePair(vecPoints))
    learntData.zipWithIndex.map { case (data, index) => normal.features(index) -> data.toString }.toMap
  }

  def learnSumSquarePair(points: Vector[Point]): SumSquarePairData = {
    SumSquarePairData(points.zipWithIndex.map(pointAndIndex => Util.sqrSum(Util.vectorToArray(points), pointAndIndex._2)).max)
  }

  override def detect(model: Map[String, String], test: TimeSeries): Vector[(String, Int)] = {
    var anomalies: Vector[(String, Int)] = Vector()
    pairs.zipWithIndex.zip(learntData).foreach((pairAndData) => {
      var pairPoints: Vector[Point] = test.getValues(test.features(pairAndData._1._1)).get.zip(
        test.getValues(test.features(pairAndData._1._2)).get).map(xAndYValues => new Point(xAndYValues._2, xAndYValues._1))
      anomalies = anomalies ++ (detectSumSquarePair(pairAndData._2, Util.vectorToArray(pairPoints)).map(index => (Util.orderByLetterOrder(test, pairAndData._1._1, pairAndData._1._2), index)))
    })
    anomalies.distinct
  }

  def detectSumSquarePair(sumSquarePairData: SumSquarePairData, testPoints: Array[Point]): Array[Int] = {
    testPoints.zipWithIndex.filter(pointAndIndex => Util.sqrSum(testPoints, pointAndIndex._2) > sumSquarePairData.maxSqrSum).map(pointAndIndex => pointAndIndex._2)
  }
}

case class SumSquarePairData(maxSqrSum: Double) extends BaseLearntPairData {}
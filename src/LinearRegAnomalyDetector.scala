

object LinearRegAnomalyDetector extends AnomalyDetector {
  var learntMap: Map[String, String] = Map()
  private var learntData: Vector[LinearRegPairData] = Vector()
  private var pairs: Array[Int] = Array()

  override def learn(normal: TimeSeries): Map[String, String] = {
    val maxCovValues: Array[MaxCov] = Util.findMaxCov(normal)
    pairs = maxCovValues.map(value => value.indexOfMaxCov)
    var pointsOfCovariancePairs: Vector[Vector[Point]] = Util.createPointsFromCovariance(maxCovValues, normal)
    learntData = pointsOfCovariancePairs.map(pointPair => learnLinearRegPair(pointPair))
    learntMap = learntData.zipWithIndex.map { case (data, index) => (normal.features(index), data.line.toString + "," + data.maxDist.toString) }.toMap
    learntMap
  }

  def learnLinearRegPair(points: Vector[Point]): LinearRegPairData = {
    var line: Line = new Line(Array.from(points))
    new LinearRegPairData(line, Util.findMaxDistance(line, Array.from(points)))
  }

  override def detect(model: Map[String, String], test: TimeSeries): Vector[(String, Int)] = {
    var anomalies: Vector[(String, Int)] = Vector()
    pairs.zipWithIndex.zip(learntData).foreach((pairAndData) => {
      val points: Vector[Point] = test.getValues(test.features(pairAndData._1._1)).get.zip(
        test.getValues(test.features(pairAndData._1._2)).get).map(xAndYValues => new Point(xAndYValues._2, xAndYValues._1))
      anomalies = anomalies :++ detectLinearRegPair(pairAndData._2, Array.from(points)).map(index => (Util.orderByLetterOrder(test, pairAndData._1._1, pairAndData._1._2), index))
    })
    anomalies.distinct
  }

  def detectLinearRegPair(linearRegPairData: LinearRegPairData, points: Array[Point]): Array[Int] = {
    points.zipWithIndex.filter(pointAndIndex => linearRegPairData.line.dist(pointAndIndex._1) > linearRegPairData.maxDist).map(pointAndIndex => pointAndIndex._2)
  }


}

case class LinearRegPairData(line: Line, maxDist: Double) extends BaseLearntPairData {}


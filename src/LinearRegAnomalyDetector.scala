import scala.collection.mutable

object LinearRegAnomalyDetector extends AnomalyDetector {
  var learntMap: Map[String, String] = Map()
  var learntTimeSeries: TimeSeries = null
  private var learntData: Vector[(Line, Double)] = Vector()
  private var pairs: Array[Int] = Array()

  override def learn(normal: TimeSeries): Map[String, String] = {
    val maxCovValues: Array[MaxCov] = Util.findMaxCov(normal)
    pairs = maxCovValues.map(value => value.indexOfMaxCov)
    var pointsOfCovariancePairs: Vector[Vector[Point]] = Util.createPointsFromCovariance(maxCovValues, normal)
    learntData = pointsOfCovariancePairs.map(pointPair=> learnLinearRegPair(pointPair))
    learntMap = learntData.zipWithIndex.map { case (data, index) => (normal.features(index), data._1.toString() + "," + data._2.toString) }.toMap
    learntMap
  }
  def learnLinearRegPair(points: Vector[Point]): (Line, Double) = {
    var line: Line = new Line(Array.from(points))
     (line, Util.findMaxDistance(line, Array.from(points)))

  }

  override def detect(model: Map[String, String], test: TimeSeries): Vector[(String, Int)] = {
    var anomalies: Vector[(String, Int)] = Vector()
    pairs.zipWithIndex.zip(learntData).foreach((pairAndData) => {
      test.getValues(test.features(pairAndData._1._1)).get.zip(
        test.getValues(test.features(pairAndData._1._2)).get).map(xAndYValues => new Point(xAndYValues._2, xAndYValues._1)).zipWithIndex
        .foreach(point => {
          if (pairAndData._2._1.dist(point._1) > pairAndData._2._2) {
            anomalies = anomalies :+ (Util.orderByLetterOrder(test, pairAndData._1._1, pairAndData._1._2), point._2)
          }
        })
    })
    anomalies.distinct
  }


}


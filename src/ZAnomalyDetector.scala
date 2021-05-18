import scala.collection.mutable
import scala.collection.mutable.ArrayBuffer

object ZAnomalyDetector extends AnomalyDetector {
  var learntMap: Map[String, String] = Map()
  var learntTimeSeries: TimeSeries = null

  override def learn(normal: TimeSeries): Map[String, String] = {
    this.learntTimeSeries = normal
    normal.features.map { case (feature) => feature -> learnZAnomalyPair(normal.getValues(feature).get).maxZScore.toString }.toMap
  }

  def learnZAnomalyPair(values: Vector[Double]): ZAnomalyPairData = {
    ZAnomalyPairData(values.map(value => Util.absZScore(values, value)).max, values)
  }

  override def detect(model: Map[String, String], test: TimeSeries): Vector[(String, Int)] = {
    var anomalies: Vector[(String, Int)] = Vector()
    test.features.foreach(feature =>
      anomalies = anomalies :++ detecZAnomalyPair(ZAnomalyPairData(model(feature).toDouble, this.learntTimeSeries.getValues(feature).get), test.getValues(feature).get).map(anomalyIndex => (feature, anomalyIndex))
    )
    anomalies
  }

  def detecZAnomalyPair(zAnomalyPairData: ZAnomalyPairData, detectValues: Vector[Double]): Array[Int] = {
    Array.from(detectValues.zipWithIndex.filter(value => Util.absZScore(zAnomalyPairData.values, value._1) > zAnomalyPairData.maxZScore).map(valueAndIndex => valueAndIndex._2))
  }
}

case class ZAnomalyPairData(maxZScore: Double, values: Vector[Double]) extends BaseLearntPairData {}

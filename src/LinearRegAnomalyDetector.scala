import scala.collection.mutable

object LinearRegAnomalyDetector extends  AnomalyDetector {
  var learntMap: Map[String, String] = Map()
  var learntTimeSeries: TimeSeries = null
  override def learn(normal: TimeSeries): Map[String, String] = {

  }

  override def detect(model: Map[String, String], test: TimeSeries): Vector[(String, Int)] = ???

}

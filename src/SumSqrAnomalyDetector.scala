
import scala.collection.mutable

object SumSqrAnomalyDetector extends  AnomalyDetector {
  override def learn(normal: TimeSeries): Map[String, String] = ???

  override def detect(model: Map[String, String], test: TimeSeries): Vector[(String, Int)] = ???
}

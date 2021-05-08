import scala.collection.mutable

object ZAnomalyDetector extends AnomalyDetector {
  var learntMap: Map[String, String] = Map()
  var learntTimeSeries: TimeSeries = null
  override def learn(normal: TimeSeries): Map[String, String] = {
    this.learntTimeSeries = normal
    normal.features.foreach(feature => learntMap = learntMap + (feature -> (normal.getValues(feature).get.map[Double](value => {
      Util.absZScore(normal.getValues(feature).get, value)
    }
    ).max.toString)))
    this.learntMap
  }

  override def detect(model: Map[String, String], test: TimeSeries): Vector[(String, Int)] = {
    var exceptions: Vector[(String, Int)] = Vector()
    test.features.foreach(feature => test.getValues(feature).get.zipWithIndex.foreach(valueOfFeature =>
      if (Util.absZScore(this.learntTimeSeries.getValues(feature).get, valueOfFeature._1) > model(feature).toDouble)
        exceptions = exceptions :+ (feature, valueOfFeature._2)))
    exceptions
  }
}

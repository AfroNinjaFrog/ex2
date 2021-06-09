
import scala.collection.mutable

object HybridAnomalyDetector extends AnomalyDetector {
  var learntData: Array[BaseLearntPairData] = Array()
  var maxCovValues: Array[MaxCov] = Array()

  override def learn(normal: TimeSeries): Map[String, String] = {
    maxCovValues = Util.findMaxCov(normal)
    var pointsOfCovariancePairs = Util.createPointsFromCovariance(maxCovValues, normal)
    maxCovValues.zipWithIndex.foreach(maxCovAndIndex => {
      if(maxCovAndIndex._1.maxCov < 0.5 || maxCovAndIndex._2 > maxCovAndIndex._1.indexOfMaxCov) {
        learntData = learntData :+ ZAnomalyDetector.learnZAnomalyPair(normal.getValues(normal.features(maxCovAndIndex._2)).get)
      } else if (maxCovAndIndex._1.maxCov >= 0.9) {
        learntData = learntData :+ LinearRegAnomalyDetector.learnLinearRegPair(pointsOfCovariancePairs(maxCovAndIndex._2))
      } else if (maxCovAndIndex._1.maxCov > 0.5) {
        val curPoints: Array[Point] = Util.vectorToArray(pointsOfCovariancePairs(maxCovAndIndex._2))
        var (minDist, minIndex) = (-1.0, 0)
        curPoints.zipWithIndex.map(points => (Util.sqrSum(curPoints, points._2), points._2)).foreach(distAndIndex => {
          if (distAndIndex._2 == 0) minDist = distAndIndex._1
          else if (minDist > distAndIndex._1) {
            minIndex = distAndIndex._2
            minDist = distAndIndex._1
          }
        })
        val centerPoint: Point = curPoints(minIndex)
        learntData = learntData :+ CirclePairData(curPoints.map(point => Util.dist(centerPoint, point)).max)
      }
    })
    learntData.zipWithIndex.map { case (data, index) => normal.features(index) -> data.toString }.toMap
  }

  override def detect(model: Map[String, String], test: TimeSeries): Vector[(String, Int)] = {
    var anomalies: Vector[(String, Int)] = Vector()
    var pointsOfCovariancePairs = Util.createPointsFromCovariance(maxCovValues, test)
    maxCovValues.zipWithIndex.foreach(maxCovAndIndex => {
      if(maxCovAndIndex._1.maxCov < 0.5 || maxCovAndIndex._2 > maxCovAndIndex._1.indexOfMaxCov) {
        anomalies = anomalies ++ (
          ZAnomalyDetector.detecZAnomalyPair(learntData(maxCovAndIndex._2).asInstanceOf[ZAnomalyPairData], test.getValues(test.features(maxCovAndIndex._2)).get)
            .map(index => (test.features(maxCovAndIndex._2), index)))
      }
      else if (maxCovAndIndex._1.maxCov >= 0.9) {
        anomalies = anomalies ++ (
          LinearRegAnomalyDetector.detectLinearRegPair(learntData(maxCovAndIndex._2).asInstanceOf[LinearRegPairData], Util.vectorToArray(pointsOfCovariancePairs(maxCovAndIndex._2)))
          .map(index => (Util.orderByLetterOrder(test, maxCovAndIndex._1.indexOfMaxCov, maxCovAndIndex._2), index)))
      } else if (maxCovAndIndex._1.maxCov > 0.5) {
        val curPoints: Array[Point] = Util.vectorToArray(pointsOfCovariancePairs(maxCovAndIndex._2))
        var (minDist, minIndex) = (-1.0, 0)
        curPoints.zipWithIndex.map(points => (Util.sqrSum(curPoints, points._2), points._2)).foreach(distAndIndex => {
          if (distAndIndex._2 == 0) minDist = distAndIndex._1
          else if (minDist > distAndIndex._1) {
            minIndex = distAndIndex._2
            minDist = distAndIndex._1
          }
        })
        val centerPoint: Point = curPoints(minIndex)
        anomalies = anomalies ++ (curPoints.zipWithIndex.map(point => (Util.dist(centerPoint, point._1), point._2)).filter(distAndIndex => distAndIndex._1 > learntData(maxCovAndIndex._2).asInstanceOf[CirclePairData].radius)
          .map(index => (Util.orderByLetterOrder(test, maxCovAndIndex._1.indexOfMaxCov, maxCovAndIndex._2), index._2)))
      }
    })
    anomalies.distinct
  }

}

case class CirclePairData(radius: Double) extends BaseLearntPairData

import scala.collection.mutable
import scala.io.{BufferedSource, Source}
import scala.util.control.Exception.allCatch

class TimeSeries(csvFileName: String) {
  val source: BufferedSource = Source.fromFile(csvFileName)
  private var mapOfFileValues: mutable.HashMap[String, Vector[Double]] = mutable.HashMap.empty
  private var prepareForFeatures: Array[String] = Array()
  source.getLines().zipWithIndex.foreach(line => {
    if (line._2 == 0) {
      prepareForFeatures = line._1.split(",")
    } else {

      line._1.split(',').zipWithIndex.foreach((value) => mapOfFileValues += (prepareForFeatures(value._2) -> (mapOfFileValues.isDefinedAt(prepareForFeatures(value._2)) match {
        case true => mapOfFileValues.get(prepareForFeatures(value._2)).get :+ value._1.toDouble
        case false => Vector[Double](value._1.toDouble)
      })))

    }
  })
  source.close()
  val features: Vector[String] = Util.arrayToVector(prepareForFeatures)

  // given name of a feature return in O(1) its value series
  def getValues(feature: String): Option[Vector[Double]] = {
    this.mapOfFileValues.get(feature)
  }

  // given name of a feature return in O(1) its value at the given time step
  def getValue(feature: String, timeStep: Int): Option[Double] = {
    allCatch.opt(this.getValues(feature).get(timeStep))
  }

  // given name of a feature return its value series in the range of indices
  def getValues(feature:String,r:Range):Option[Vector[Double]]= {
    if(r.max >= this.getValues(feature).get.length || r.min < 0) {
      return None
    }
    allCatch.opt(this.getValues(feature).get.zipWithIndex.filter(item=> item._2 <= r.max && item._2 >= r.min ).map((item => item._1)))
  }


}

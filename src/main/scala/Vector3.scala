import scala.math.pow
import scala.math.sqrt

case class Vector3(x: Float, y: Float, z: Float)  {
  def + (that: Vector3) : Vector3 = Vector3(x + that.x, y + that.y, z + that.z)
  def - (that: Vector3) : Vector3 = Vector3(x - that.x, y - that.y, z - that.z)
  def * (that: Vector3) : Vector3 = Vector3(x * that.x, y * that.y, z * that.z)
  def * (that: Float) : Vector3 = Vector3(that*x, that*y, that*z)
  def / (that: Vector3) : Vector3 = Vector3(x / that.x, y / that.y, z / that.z)
  def / (that: Float) : Vector3  = this * (1 / that)

  def squared_length: Float = (scala.math.pow(x,2) + scala.math.pow(y,2) + scala.math.pow(z,2)).toFloat

  def length : Float = scala.math.sqrt(scala.math.pow(x,2) + scala.math.pow(y,2) + scala.math.pow(z,2)).toFloat

  def normalized : Vector3 = this / this.length

  def unary_- : Vector3 = Vector3(-x, -y, -z)

  def dot (that: Vector3): Float = x * that.x + y * that.y + z * that.z

  def cross (that: Vector3): Vector3 = Vector3(y * that.z - z * that.y,
                                               z * that.x - x * that.z,
                                               x * that.y - y * that.x)

  def intPrint : String = x.toInt+" "+y.toInt+" "+z.toInt+"\n"

  def elementwise_pow(exponent: Double) : Vector3 = Vector3(
    scala.math.pow(x,exponent).toFloat,
    scala.math.pow(y, exponent).toFloat,
    scala.math.pow(z, exponent).toFloat)

  override def toString: String = x+" "+y+" "+z+"\n"

}


//import javafx.css.converter.DeriveColorConverter
case class Vector3(x: Float, y: Float, z: Float)  {
  def + (that: Vector3) : Vector3 = Vector3(x + that.x, y + that.y, z + that.z)
  def - (that: Vector3) : Vector3 = Vector3(x - that.x, y - that.y, z - that.z)
  def * (that: Vector3) : Vector3 = Vector3(x * that.x, y * that.y, z * that.z)
  def * (that: Float) : Vector3 = Vector3(that*x, that*y, that*z)
  def / (that: Vector3) : Vector3 = Vector3(x / that.x, y / that.y, z / that.z)
  def / (that: Float) : Vector3  = this * (1 / that)
  def unary_- : Vector3 = Vector3(-x, -y, -z)
  def dot (that: Vector3): Float = x * that.x + y * that.y + z * that.z
  def cross (that: Vector3): Vector3 = Vector3(y * that.z - z * that.y,
    z * that.x - x * that.z,
    x * that.y - y * that.x)
  def elementwise_pow(exponent: Double) : Vector3 = Vector3(
    scala.math.pow(x,exponent).toFloat,
    scala.math.pow(y, exponent).toFloat,
    scala.math.pow(z, exponent).toFloat)
  def max : Float = scala.math.max(scala.math.max(x,y),z)

  def squared_length: Float = (scala.math.pow(x,2) + scala.math.pow(y,2) + scala.math.pow(z,2)).toFloat
  def length : Float = scala.math.sqrt(scala.math.pow(x,2) + scala.math.pow(y,2) + scala.math.pow(z,2)).toFloat
  lazy val normalized : Vector3 = this / this.length

  def intPrint : String = x.toInt+" "+y.toInt+" "+z.toInt+"\n"

  override def toString: String = x+" "+y+" "+z+"\n"

}
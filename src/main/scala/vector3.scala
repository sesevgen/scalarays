import scala.math.pow
import scala.math.sqrt

case class vector3 (x: Float, y: Float, z: Float)  {
  def + (that: vector3) : vector3 = vector3(x + that.x, y + that.y, z + that.z)
  def - (that: vector3) : vector3 = vector3(x - that.x, y - that.y, z - that.z)
  def * (that: vector3) : vector3 = vector3(x * that.x, y * that.y, z * that.z)
  def * (that: Float) : vector3 = vector3(that*x, that*y, that*z)
  def / (that: vector3) : vector3 = vector3(x / that.x, y / that.y, z / that.z)
  def / (that: Float) : vector3  = this * (1 / that)

  def squared_length: Float = (scala.math.pow(x,2) + scala.math.pow(y,2) + scala.math.pow(z,2)).toFloat

  def length : Float = scala.math.sqrt(scala.math.pow(x,2) + scala.math.pow(y,2) + scala.math.pow(z,2)).toFloat

  def normalized : vector3 = this / this.length
  
  def unary_- : vector3 = vector3(-x, -y, -z)
  
  def dot (that: vector3): Float = x * that.x + y * that.y + z * that.z
  
  def cross (that: vector3): vector3 = vector3(y * that.z - z * that.y,
                                               z * that.x - x * that.z,
                                               x * that.y - y * that.x)

  def intPrint : String = x.toInt+" "+y.toInt+" "+z.toInt+"\n"
                                               
  override def toString: String = x+" "+y+" "+z+"\n"
 
}


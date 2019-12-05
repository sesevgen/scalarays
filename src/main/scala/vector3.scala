import scala.math.pow
import scala.math.sqrt

case class vector3 (x: Double, y: Double, z: Double)  {
  def + (that: vector3) : vector3 = vector3(x + that.x, y + that.y, z + that.z)
  def - (that: vector3) : vector3 = vector3(x - that.x, y - that.y, z - that.z)
  def * (that: vector3) : vector3 = vector3(x * that.x, y * that.y, z * that.z)
  def * (that: Double) : vector3 = vector3(that*x, that*y, that*z)
  def / (that: vector3) : vector3 = vector3(x / that.x, y / that.y, z / that.z)
  def / (that: Double) : vector3  = this * (1 / that)

  def squared_length: Double = scala.math.pow(x,2) + scala.math.pow(y,2) + scala.math.pow(z,2)

  def length : Double = scala.math.sqrt(scala.math.pow(x,2) + scala.math.pow(y,2) + scala.math.pow(z,2))

  def normalized : vector3 = this / this.length
  
  def unary_- : vector3 = vector3(-x, -y, -z)
  
  def dot (that: vector3): Double = x * that.x + y * that.y + z * that.z
  
  def cross (that: vector3): vector3 = vector3(y * that.z - z * that.y,
                                               z * that.x - x * that.z,
                                               x * that.y - y * that.x)
                                               
  override def toString = "("+x+","+y+","+z+")"
 
}


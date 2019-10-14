case class vector3 (x: Double, y: Double, z: Double)  {
  def + (that: vector3): vector3 = vector3(x + that.x, y + that.y, z + that.z)
  def - (that: vector3): vector3 = vector3(x - that.x, y - that.y, z - that.z)
  
  def unary_- : vector3 = vector3(-x, -y, -z)
  
  def dot (that: vector3): Double = x * that.x + y * that.y + z * that.z
  
  def cross (that: vector3): vector3 = vector3(y * that.z - z * that.y,
                                               z * that.x - x * that.z,
                                               x * that.y - y * that.x)
                                               
  override def toString = "("+x+","+y+","+z+")"
 
}


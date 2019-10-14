case class ray (a: vector3, b: vector3) {
  implicit class scaler(value: Double) {
    def * (that: vector3): vector3 = vector3(value * that.x, value * that.y, value * that.z) 
  }
    
  def origin = a
  def direction = b
  def point_at_parameter(t: scaler) = a + t * b
}
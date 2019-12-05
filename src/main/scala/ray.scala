case class ray (a: vector3, b: vector3) {
    
  def origin = a
  def direction = b
  def point_at_parameter(t: Double) = a + b * t
}
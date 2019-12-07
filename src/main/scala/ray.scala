case class ray (a: vector3, b: vector3) {
    
  def origin: vector3 = a
  def direction: vector3 = b
  def point_at_parameter(t: Float): vector3 = a + b * t
}
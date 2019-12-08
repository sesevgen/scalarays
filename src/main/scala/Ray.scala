case class Ray(origin: Vector3, direction: Vector3) {
  def point_at_parameter(t: Float): Vector3 = origin + direction * t
}
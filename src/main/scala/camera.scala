case class Camera (lower_left_corner: Vector3, horizontal: Vector3, vertical: Vector3, origin:Vector3) {
  def get_ray(u: Float, v: Float) : Ray = {
    Ray(origin, lower_left_corner + horizontal * u +vertical * v - origin)}
}

case class Camera (origin: Vector3,
                   lookat: Vector3,
                   vup: Vector3,
                   vfov: Float,
                   aspect: Float,
                   aperture: Float,
                   focus_dist: Float)
{
  def random_in_unit_disk: Vector3 = {
    val p = Vector3(scala.util.Random.nextFloat(), scala.util.Random.nextFloat(), 0f) * 2.0f - Vector3(1f, 1f, 0)
    if (p.squared_length < 1.0) p else random_in_unit_disk
  }

  val lens_radius = aperture/2

  val theta = vfov * 3.14159 / 180
  val half_height = scala.math.tan(theta/2).toFloat
  val half_width = aspect * half_height

  val w = (origin - lookat).normalized
  val u = (vup cross w).normalized
  val v = w cross u

  val lower_left_corner = origin - (u * half_width * focus_dist) - (v * half_height * focus_dist) - w * focus_dist
  val horizontal = u*half_width*focus_dist*2
  val vertical = v*half_height*focus_dist*2

  def get_ray(s: Float, t: Float) : Ray = {
    val rd = random_in_unit_disk * lens_radius
    val offset = u * rd.x + v * rd.y
    Ray(origin + offset, lower_left_corner + horizontal * s +vertical * t - origin - offset)
  }
}

import scala.annotation.tailrec

case class Camera (origin: Vector3,
                   lookat: Vector3,
                   vup: Vector3,
                   vfov: Float,
                   aspect: Float,
                   aperture: Float,
                   focus_dist: Float)
{
  @tailrec
  final def random_in_unit_disk: Vector3 = {
    val p = Vector3(scala.util.Random.nextFloat(), scala.util.Random.nextFloat(), 0f) * 2.0f - Vector3(1f, 1f, 0)
    if (p.squared_length < 1.0) p else random_in_unit_disk
  }

  val lens_radius: Float = aperture/2

  val theta: Float = vfov * 3.14159f / 180f
  val half_height: Float = scala.math.tan(theta/2).toFloat
  val half_width: Float = aspect * half_height

  val w: Vector3 = (origin - lookat).normalized
  val u: Vector3 = (vup cross w).normalized
  val v: Vector3 = w cross u

  val lower_left_corner: Vector3 = origin - (u * half_width * focus_dist) - (v * half_height * focus_dist) - w * focus_dist
  val horizontal: Vector3 = u*half_width*focus_dist*2
  val vertical: Vector3 = v*half_height*focus_dist*2

  def get_ray(s: Float, t: Float) : Ray = {
    val rd = random_in_unit_disk * lens_radius
    val offset = u * rd.x + v * rd.y
    Ray(origin + offset, lower_left_corner + horizontal * s +vertical * t - origin - offset)
  }
}

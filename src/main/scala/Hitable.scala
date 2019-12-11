abstract class Hitable{
  val material : Materialtype
  def intersect(r: Ray, tmin: Float, tmax: Float): Option[(Float, Ray, Vector3)]
}

case class Sphere(center: Vector3, radius: Float, material: Materialtype) extends Hitable {
  def intersect(r: Ray, tmin: Float, tmax: Float): Option[(Float, Ray, Vector3)] = {
    val oc = r.origin - center
    val a = r.direction.squared_length
    val b = oc dot r.direction
    val c = oc.squared_length - radius * radius
    val discriminant = b * b - a * c
    if (discriminant < 0) None
    else
    {
      val temp = (-b -scala.math.sqrt(discriminant)).toFloat / a
      if ((tmin < temp) && (tmax > temp)) {
        val p = r.point_at_parameter(temp)
        val n = (p - center) / radius
        Some((temp, material.scatter(r, p, n), material.albedo))
      }
      else {
        val temp2 = (-b +scala.math.sqrt(discriminant)).toFloat / a
        if ((tmin < temp2) && (tmax > temp2)) {
          val p = r.point_at_parameter(temp2)
          val n = (p - center) / radius
          return Some((temp2, material.scatter(r, p, n), material.albedo))
        }
        None
      }
    }
  }
}
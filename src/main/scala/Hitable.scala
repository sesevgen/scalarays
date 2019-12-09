abstract class Hitable{
  val material : Materialtype
  def intersect(r: Ray, tmin: Float, tmax: Float): (Float, Hitable)
  def propagate_ray(r: Ray, t: Float): Ray
}

case class Sphere(center: Vector3, radius: Float, material: Materialtype) extends Hitable {
  def intersect(r: Ray, tmin: Float, tmax: Float): (Float, Sphere) = {
    val oc = r.origin - center
    val a = r.direction.squared_length
    val b = oc dot r.direction
    val c = oc.squared_length - radius * radius
    val discriminant = b * b - a * c
    if (discriminant < 0)
      {(-1.0f, this)}
    else
      {
        val temp = (-b -scala.math.sqrt(discriminant)).toFloat / a
        if ((tmin < temp) && (tmax > temp)) {
          val p = r.point_at_parameter(temp)
          val n = (p - center) / radius
          return(temp, this) //temp, material.scatter(r, p, n) )
          //return (true, HitRec(temp, r.point_at_parameter(temp), (r.point_at_parameter(temp) - center) / radius, material) )
          // return (true, material.scatter(r.point_at_parameter(temp), (r.point_at_parameter(temp) - center) / radius))
        }
        else {
          val temp2 = (-b +scala.math.sqrt(discriminant)).toFloat / a
          if ((tmin < temp2) && (tmax > temp2)) {
            val p = r.point_at_parameter(temp2)
            val n = (p - center) / radius
            return(temp, this) //temp, material.scatter(r, p, n) )
            //return (true, HitRec(temp2, r.point_at_parameter(temp2), (r.point_at_parameter(temp2) - center) / radius , material) )
          }
        }
        {(-1.0f, this)}
      }
  }
  def propagate_ray(r: Ray, t: Float): Ray = {
    val p = r.point_at_parameter(t)
    val n = (p - center) / radius
    material.scatter(r, p, n)
  }
}
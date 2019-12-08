case class HitRec(t: Float, p: Vector3, normal: Vector3){
  override def toString: String = t.toString + p.toString + normal.toString
}

abstract class Hitable {
  def intersect(r: Ray, tmin: Float, tmax: Float): (Boolean, HitRec)
}

case class Sphere(center: Vector3, radius: Float) extends Hitable {
  def intersect(r: Ray, tmin: Float, tmax: Float): (Boolean, HitRec) = {
    val oc = r.origin - center
    val a = r.direction.squared_length
    val b = oc dot r.direction
    val c = oc.squared_length - radius * radius
    val discriminant = b * b - a * c
    if (discriminant < 0)
      {(false, HitRec(0, Vector3(0,0,0), Vector3(0,0,0)))}
    else
      {
        val temp = (-b -scala.math.sqrt(discriminant)).toFloat / a
        if ((tmin < temp) && (tmax > temp)) {
          return (true, HitRec(temp, r.point_at_parameter(temp), (r.point_at_parameter(temp) - center) / radius))
        }
        else {
          val temp2 = (-b +scala.math.sqrt(discriminant)).toFloat / a
          if ((tmin < temp2) && (tmax > temp2)) {
            return (true, HitRec(temp2, r.point_at_parameter(temp2), (r.point_at_parameter(temp2) - center) / radius ))
          }
        }
        (false, HitRec(0, Vector3(0,0,0), Vector3(0,0,0)))
      }
  }
}
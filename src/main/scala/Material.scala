abstract class Materialtype {
  def random_in_scale_sphere(scale: Float) : Vector3 = {
    val rand = scala.util.Random
    val randvec = Vector3(rand.nextFloat()-0.5f, rand.nextFloat()-0.5f, rand.nextFloat()-0.5f)*2.0f*scale
    if (randvec.squared_length < scale) randvec else random_in_scale_sphere(scale)
  }
  def scatter(r_in: Ray, contact: Vector3, normal: Vector3) : (Vector3, Ray)
}

case class Lambertian (albedo: Vector3) extends Materialtype {
  def scatter(r_in: Ray, contact: Vector3, normal: Vector3): (Vector3, Ray) = {
    val target = contact + normal + random_in_scale_sphere(1.0f)
    val scattered = Ray(contact, target-contact)
    (albedo, scattered)
  }
}

case class Metal (albedo: Vector3, diffuse: Float) extends Materialtype {
  def reflect(v: Vector3, n: Vector3): Vector3 = { v - (n * (v dot n) ) * 2.0f}
  def scatter(r_in: Ray, contact: Vector3, normal: Vector3): (Vector3, Ray) = {
    val target = reflect(r_in.direction.normalized, normal) + random_in_scale_sphere(diffuse)
    val scattered = Ray(contact, target-contact)
    (albedo, scattered)
  }
}
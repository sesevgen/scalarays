abstract class Materialtype {
  val albedo: Vector3
  def random_in_scale_sphere(scale: Float) : Vector3 = {
    val rand = scala.util.Random
    val randvec = Vector3(rand.nextFloat()-0.5f, rand.nextFloat()-0.5f, rand.nextFloat()-0.5f)*2.0f*scale
    if (randvec.squared_length < scale)
      return randvec
    random_in_scale_sphere(scale)
  }
  def scatter(r_in: Ray, contact: Vector3, normal: Vector3) : Ray
}

case class Lambertian (albedo: Vector3) extends Materialtype {
  def scatter(r_in: Ray, contact: Vector3, normal: Vector3): Ray = {
    val target = contact + normal + random_in_scale_sphere( scale = 1.0f )
    Ray(contact, target-contact)
  }
}

case class Metal (albedo: Vector3, diffuse: Float) extends Materialtype {
  def reflect(v: Vector3, n: Vector3): Vector3 = { v - (n * (v dot n) ) * 2.0f}
  def scatter(r_in: Ray, contact: Vector3, normal: Vector3): Ray = {
    val target = reflect(r_in.direction.normalized, normal) + random_in_scale_sphere(diffuse)
    Ray(contact, target)
  }
}

case class Dielectric (albedo: Vector3, r_idx: Float, diffuse: Float) extends Materialtype {
  def reflect(v: Vector3, n: Vector3): Vector3 = {
    v - (n * (v dot n)) * 2.0f
  }

  def schlick(cosine: Float, ref_idx: Float): Float = {
    val r0 = scala.math.pow((1 - ref_idx) / (1 + ref_idx), 2)
    (r0 + (1 - r0) * scala.math.pow((1 - cosine), 5)).toFloat
  }

  def nidx_ratio_cosine(r_in: Ray, normal: Vector3): (Boolean, Float, Float) = {
    if ((r_in.direction.normalized dot normal) > 0.0f) {
      (false, r_idx, (r_in.direction.normalized dot normal) * r_idx)
    }
    else {
      (true, 1.0f / r_idx, -((r_in.direction.normalized dot normal) * r_idx))
    }
  }

  def refract(internal: Boolean, v: Vector3, normal: Vector3, nidx_ratio: Float): (Boolean, Vector3) = {
    val n = if (internal) normal else -normal
    val uv = v.normalized
    val dt = uv dot n
    val discriminant = 1.0f - nidx_ratio * nidx_ratio * (1 - dt * dt)
    if (discriminant > 0) {
      (true, (uv - n * dt) * nidx_ratio - n * scala.math.sqrt(discriminant).toFloat)
    }
    else {
      (false, Vector3(0, 0, 0))
    }
  }

  def scatter(r_in: Ray, contact: Vector3, normal: Vector3): Ray = {
    val (internal, nidx_ratio, cosine) = nidx_ratio_cosine(r_in, normal)
    val (refracted, refraction) = refract(internal, r_in.direction.normalized, normal, nidx_ratio)

    val reflect_prob = if (refracted) schlick(cosine, r_idx) else 1.0f

    if (scala.util.Random.nextFloat() < reflect_prob) {
      val target = reflect(r_in.direction.normalized, normal) + random_in_scale_sphere(diffuse)
      Ray(contact, target)
    }
    else {
      Ray(contact, refraction)
    }
  }
}
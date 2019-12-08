import java.io.File
import java.io.PrintWriter
import scala.math.sqrt
import scala.util.Random

object Main extends App {

  val tmin = 0.001f
  val tmax = scala.Float.MaxValue

  def random_in_unit_sphere() : Vector3 = {
    val rand = scala.util.Random
    val randvec = Vector3(rand.nextFloat()-0.5f, rand.nextFloat()-0.5f, rand.nextFloat()-0.5f)*2.0f
    if (randvec.squared_length < 1.0) randvec else random_in_unit_sphere
  }

  def sky_color(r: Ray): Vector3 = {
    val unit_direction = r.direction.normalized
    val t: Float = 0.5f * (unit_direction.y + 1.0f)
    Vector3(1.0f, 1.0f, 1.0f) * (1.0f - t) + Vector3(0.5f, 0.7f, 1.0f) * t
  }

  def color(r: Ray, objects: List[Hitable]): Vector3 = {
    val results = objects.map(_.intersect(r, tmin, tmax)) filter (_._1) map (_._2)
    if (results.length > 0) {
      val rec = results.head
      //Vector3(rec.normal.x + 1, rec.normal.y + 1, rec.normal.z + 1) * 0.5f
      val target = rec.p + rec.normal + random_in_unit_sphere()
      color(Ray(rec.p, target - rec.p), objects) * 0.5f
    }
    else {
      sky_color(r)
    }
  }

  def process_location(u: Float, v: Float, canvas: Canvas, camera: Camera, objects: List[Hitable]): Vector3 = {
    val r = camera.get_ray(u, v)
    color(r, objects)
  }

  def process_with_subsample(x: Int, y: Int, canvas: Canvas, camera: Camera, objects: List[Hitable], samples: Int) : Vector3 = {
    val rand = scala.util.Random
    val subsample_locs: Vector[(Float, Float)] = Vector.tabulate(samples)(_ =>
      ( (x+rand.nextFloat()) / canvas.nx.toFloat, (y+rand.nextFloat()) / canvas.ny.toFloat))
    subsample_locs.map(x => process_location(x._1, x._2, canvas, camera, objects)).reduceLeft(_ + _) / samples.toFloat
  }

  def draw(canvas: Canvas, camera: Camera, objects: List[Hitable], subsampling: Int, bit_depth: Float, gamma_corr: Double) = {
    val writer = new PrintWriter(new File("Test2.ppm"))
    writer.write("P3\n" + canvas.nx + " " + canvas.ny + "\n255\n")
    val yrange = List.range(canvas.ny - 1, -1, -1)
    val xrange = List.range(0, canvas.nx, 1)
    for {
      y <- yrange
      x <- xrange
    }
      yield
        {
          val colorscale = process_with_subsample(x, y, canvas, camera, objects, subsampling).elementwise_pow(gamma_corr)*bit_depth
          writer.write(colorscale.intPrint)
        }
    writer.close()
  }

  val canvas = Canvas(500, 250)
  val camera = Camera(
    lower_left_corner = Vector3(-2, -1, -1),
    horizontal = Vector3(4, 0, 0),
    vertical = Vector3(0, 2, 0),
    origin = Vector3(0, 0, 0)
  )
  val objects: List[Hitable] = List(Sphere(Vector3(0, 0, -1), 0.5f), Sphere(Vector3(0, -100.5f, -1), 100f))
  println("trial" + objects)

  draw(canvas, camera, objects, subsampling = 500, bit_depth = 255.99f, gamma_corr = 0.5)
}

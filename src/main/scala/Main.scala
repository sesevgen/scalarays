import java.io.File
import java.io.PrintWriter

import scala.math.sqrt
import scala.util.Random
import scala.concurrent.{Await, Future}
import scala.concurrent.duration.Duration
import scala.concurrent.ExecutionContext.Implicits.global

object Main extends App {

  val tmin = 0.001f
  val tmax = scala.Float.MaxValue


  def random_in_unit_sphere(): Vector3 = {
    val rand = scala.util.Random
    val randvec = Vector3(rand.nextFloat() - 0.5f, rand.nextFloat() - 0.5f, rand.nextFloat() - 0.5f) * 2.0f
    if (randvec.squared_length < 1.0) randvec else random_in_unit_sphere
  }

  def sky_color(r: Ray): Vector3 = {
    val unit_direction = r.direction.normalized
    val t: Float = 0.5f * (unit_direction.y + 1.0f)
    Vector3(1.0f, 1.0f, 1.0f) * (1.0f - t) + Vector3(0.5f, 0.7f, 1.0f) * t
  }

  def color(r: Ray, objects: List[Hitable], depth: Int, max_depth: Int): Vector3 = {
    val new_depth = depth + 1
    if (new_depth > max_depth) return Vector3(0f, 0f, 0f)
    val results = objects.map(_.intersect(r, tmin, tmax)) filter (_._1 > tmin) sortBy (_._1)
    if (results.nonEmpty) {
      //Vector3(rec.normal.x + 1, rec.normal.y + 1, rec.normal.z + 1) * 0.5f
      //val rec = results.head
      results.head._2.material.albedo * color(results.head._2.propagate_ray(r, results.head._1), objects, new_depth, max_depth)
    }
    else {
      sky_color(r)
    }
  }

  def process_location(u: Float, v: Float, canvas: Canvas, camera: Camera, objects: List[Hitable], max_depth: Int): Vector3 = {
    val r = camera.get_ray(u, v)
    color(r, objects, depth = 0, max_depth)
  }

  def process_with_subsample(x: Int, y: Int,
                             canvas: Canvas, camera: Camera, objects: List[Hitable],
                             samples: Int, max_depth: Int): Vector3 = {
    val rand = scala.util.Random
    val subsample_locs: Vector[(Float, Float)] = Vector.tabulate(samples)(_ =>
      ((x + rand.nextFloat()) / canvas.nx.toFloat, (y + rand.nextFloat()) / canvas.ny.toFloat))
    subsample_locs.map(x => process_location(x._1, x._2, canvas, camera, objects, max_depth)).reduceLeft(_ + _) / samples.toFloat
  }

  def draw(canvas: Canvas,
           camera: Camera,
           objects: List[Hitable],
           subsampling: Int,
           bit_depth: Float,
           gamma_corr: Double,
           max_depth: Int) = {
    val yrange = List.range(canvas.ny - 1, -1, -1)
    val xrange = List.range(0, canvas.nx, 1)
    for {
      y <- yrange
      x <- xrange
    }
      yield {
        process_with_subsample(
          x, y,
          canvas, camera, objects,
          subsampling, max_depth).elementwise_pow(gamma_corr) * bit_depth
        //writer.write(colorscale.intPrint)
      }
    //writer.close()
  }

  val canvas = Canvas(400, 200)
  val camera = Camera(
    lower_left_corner = Vector3(-2, -1, -1),
    horizontal = Vector3(4f, 0f, 0f),
    vertical = Vector3(0f, 2f, 0f),
    origin = Vector3(0f, 0f, 0f)
  )
  val objects: List[Hitable] = List(
    Sphere(Vector3(0f, 0f, -1f), 0.5f, Lambertian(Vector3(0.8f, 0.3f, 0.3f))),
    Sphere(Vector3(0f, -100.5f, -1f), 100f, Lambertian(Vector3(0.8f, 0.8f, 0.0f))),
    Sphere(Vector3(1f, 0f, -1f), 0.5f, Metal(Vector3(0.8f, 0.6f, 0.2f), 0.01f)),
    Sphere(Vector3(-1f, 0f, -1f), 0.5f, Metal(Vector3(0.8f, 0.8f, 0.8f), 0.5f)),
  )
  println("trial" + objects)

  val writer = new PrintWriter(new File("Test2.ppm"))
  writer.write("P3\n" + canvas.nx + " " + canvas.ny + "\n255\n")
  val pixels = draw(canvas, camera, objects, subsampling = 50, bit_depth = 255.99f, gamma_corr = 0.5, max_depth = 50)
  for (vec <- pixels) {writer.write(vec.intPrint) }
  writer.close()
  }


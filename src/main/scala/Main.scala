import java.io.File
import java.io.PrintWriter

import scala.annotation.tailrec
import scala.collection.parallel.immutable.ParVector
import scala.math.sqrt
import scala.util.Random
import scala.concurrent.{Await, Future}
import scala.concurrent.duration.Duration
import scala.concurrent.ExecutionContext.Implicits.global

object Main extends App {

  val tmin = 0.001f
  val tmax = scala.Float.MaxValue


  def sky_color(r: Ray): Vector3 = {
    val unit_direction = r.direction.normalized
    val t: Float = 0.5f * (unit_direction.y + 1.0f)
    Vector3(1.0f, 1.0f, 1.0f) * (1.0f - t) + Vector3(0.5f, 0.7f, 1.0f) * t
  }

  @tailrec
  def color(r: Ray, objects: List[Hitable], depth: Int, max_depth: Int, current_albedo: Vector3): Vector3 = {
    val new_depth = depth + 1
    if (new_depth > max_depth) return Vector3(0f, 0f, 0f)
    //val results = objects.map(_.intersect(r, tmin, tmax)) filter (_._1 > tmin) sortBy (_._1)
    val results = objects.flatMap(_.intersect(r, tmin, tmax)) sortBy(_._1)
    results match {
      case Nil => current_albedo * sky_color(r)
      case x :: _ => color(x._2, objects, new_depth, max_depth, x._3 * current_albedo)
    }
  }

  def process_location(u: Float, v: Float, canvas: Canvas, camera: Camera, objects: List[Hitable], max_depth: Int): Vector3 = {
    val r = camera.get_ray(u, v)
    color(r, objects, depth = 0, max_depth, Vector3(1,1,1))
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
           max_depth: Int): Array[Future[Vector3]] = {
    val yrange = Array.range(canvas.ny - 1, -1, -1)
    val xrange = Array.range(0, canvas.nx, 1)
    for {
      y <- yrange
      x <- xrange
    }
      yield Future {
        process_with_subsample(
          x, y,
          canvas, camera, objects,
          subsampling, max_depth)
      }
  }



  // Canvas
  val (ny, nx) = (600, 400)

  // Camera
  val lookfrom = Vector3(14f, 2f, 4f)
  val lookat = Vector3(2f, 0.5f, 1f)
  val aperture = 0.1f
  val dist_to_focus = (lookfrom-lookat).length
  val vfov = 20

  // Computation
  val samples = 100
  val max_bounces = 50
  val gamma_corr = 0.5
  val bit_depth = 255.99f


  val canvas = Canvas(ny, nx)
  val camera = Camera(lookfrom, lookat, Vector3(0f, 1f, 0f), vfov, ny.toFloat/nx.toFloat, aperture, dist_to_focus)

  def random_obj (a: Int, b: Int): Option[Hitable] = {
    val rand = scala.util.Random
    val choose_mat = rand.nextFloat()
    val center = Vector3(a+0.9f*rand.nextFloat(), 0.2f, b + 0.9f*rand.nextFloat())
    if ((center - Vector3(4f, 0.2f, 0f)).length > 0.9f)
      {
        if (choose_mat < 0.75)
          {
            Some( Sphere(center, 0.2f, Lambertian(
              Vector3(rand.nextFloat()*rand.nextFloat(),
                      rand.nextFloat()*rand.nextFloat(),
                      rand.nextFloat()*rand.nextFloat()))) )
          }
        else if (choose_mat < 0.9)
          {
            Some( Sphere(center, 0.2f, Metal(
              Vector3(0.5f*(1+rand.nextFloat()),
                      0.5f*(1+rand.nextFloat()),
                      0.5f*(1+rand.nextFloat())), 0.5f*rand.nextFloat())) )
          }
        else
          {
            Some( Sphere(center, 0.2f, Dielectric(Vector3(1.0f, 1.0f, 1.0f), 1.5f, 0.01f)) )
          }
      }
    else None
  }

//    val objects: List[Hitable] = List(
//      Sphere(Vector3(0f, 0.2f, -1f), 0.2f, Lambertian(Vector3(0.8f, 0.3f, 0.3f))),
//      //Sphere(Vector3(0f, -100.5f, -1f), 100f, Lambertian(Vector3(0.8f, 0.8f, 0.0f))),
//      Sphere(Vector3(0f, -1000f, 0f), 1000f, Lambertian(Vector3(0.5f, 0.5f, 0.5f))),
//      Sphere(Vector3(1f, 0.2f, -1f), 0.2f, Metal(Vector3(0.8f, 0.6f, 0.2f), 0.01f)),
//      Sphere(Vector3(-1f, 0.2f, -1f), 0.2f, Metal(Vector3(0.8f, 0.8f, 0.8f), 0.2f)),
//      Sphere(Vector3(0f, 0.2f, -0.2f), 0.1f, Dielectric(Vector3(1f, 1f, 1f), 1.5f, 0.01f)),
//    )

  val xobj = List.range(-11,11)
  val yobj = List.range(-11,11)
  val objmap = for
    {a <- xobj
     b <- yobj} yield (a,b)

  val rand_objects = objmap.flatMap( coord => random_obj(coord._1, coord._2) )
  val other_objects = List(
    Sphere(Vector3(0f, -1000f, 0f), 1000f, Lambertian(Vector3(0.5f, 0.5f, 0.5f))),
    Sphere(Vector3(0f, 1f, 0f), 1f, Dielectric(Vector3(1f, 1f, 1f), 1.5f, 0.01f)),
    Sphere(Vector3(-4f, 1f, 0f), 1f, Lambertian(Vector3(0.4f, 0.2f, 0.1f))),
    Sphere(Vector3(4f, 1f, 0f), 1f, Metal(Vector3(0.7f, 0.6f, 0.5f), 0.01f)),
  )

  val objects = rand_objects ++ other_objects

  val t1 = System.nanoTime()
  println("trial" + objects)

  val writer = new PrintWriter(new File("Test2.ppm"))
  writer.write("P3\n" + canvas.nx + " " + canvas.ny + "\n255\n")

  val final_pixels = draw(canvas, camera, objects, subsampling = samples, max_depth = max_bounces).map(Await.result(_, Duration.Inf).elementwise_pow(gamma_corr) * bit_depth)
  for (vec <- final_pixels) { writer.write(vec.intPrint) }

  writer.close()
  println((System.nanoTime - t1) / 1e9d + " seconds")
  }


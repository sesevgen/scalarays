import java.io.File
import java.io.PrintWriter

import scala.collection.parallel.immutable.ParVector
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

  def color(r: Ray, objects: Vector[Hitable], depth: Int, max_depth: Int): Vector3 = {
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

  def process_location(u: Float, v: Float, canvas: Canvas, camera: Camera, objects: Vector[Hitable], max_depth: Int): Vector3 = {
    val r = camera.get_ray(u, v)
    color(r, objects, depth = 0, max_depth)
  }

  def process_with_subsample(x: Int, y: Int,
                             canvas: Canvas, camera: Camera, objects: Vector[Hitable],
                             samples: Int, max_depth: Int): Vector3 = {
    val rand = scala.util.Random
    val subsample_locs: Vector[(Float, Float)] = Vector.tabulate(samples)(_ =>
      ((x + rand.nextFloat()) / canvas.nx.toFloat, (y + rand.nextFloat()) / canvas.ny.toFloat))
    subsample_locs.map(x => process_location(x._1, x._2, canvas, camera, objects, max_depth)).reduceLeft(_ + _) / samples.toFloat
  }

  def draw(canvas: Canvas,
           camera: Camera,
           objects: Vector[Hitable],
           subsampling: Int,
           max_depth: Int) = {
    val yrange = Vector.range(canvas.ny - 1, -1, -1)
    val xrange = Vector.range(0, canvas.nx, 1)
    for {
      y <- yrange
      x <- xrange
    }
      yield {
        process_with_subsample(
          x, y,
          canvas, camera, objects,
          subsampling, max_depth)
      }
  }



  // Canvas
  val (ny, nx) = (900, 600)

  // Camera
  val lookfrom = Vector3(14f, 2f, 4f)
  val lookat = Vector3(2f, 0.5f, 1f)
  val aperture = 0.1f
  val dist_to_focus = (lookfrom-lookat).length
  val vfov = 20

  // Computation
  val samples = 50
  val max_bounces = 50
  val gamma_corr = 0.5
  val bit_depth = 255.99f


  val canvas = Canvas(ny, nx)
  val camera = Camera(lookfrom, lookat, Vector3(0f, 1f, 0f), vfov, ny.toFloat/nx.toFloat, aperture, dist_to_focus)

  def random_obj (a: Int, b: Int): Either[Unit, Hitable] = {
    val rand = scala.util.Random
    val choose_mat = rand.nextFloat()
    val center = Vector3(a+0.9f*rand.nextFloat(), 0.2f, b + 0.9f*rand.nextFloat())
    if ((center - Vector3(4f, 0.2f, 0f)).length > 0.9f)
      {
        if (choose_mat < 0.75)
          {
            Right( Sphere(center, 0.2f, Lambertian(
              Vector3(rand.nextFloat()*rand.nextFloat(),
                      rand.nextFloat()*rand.nextFloat(),
                      rand.nextFloat()*rand.nextFloat()))) )
          }
        else if (choose_mat < 0.9)
          {
            Right( Sphere(center, 0.2f, Metal(
              Vector3(0.5f*(1+rand.nextFloat()),
                      0.5f*(1+rand.nextFloat()),
                      0.5f*(1+rand.nextFloat())), 0.5f*rand.nextFloat())) )
          }
        else
          {
            Right( Sphere(center, 0.2f, Dielectric(Vector3(1.0f, 1.0f, 1.0f), 1.5f, 0.01f)) )
          }
      }
    else Left(Unit)
  }

//    val objects: Vector[Hitable] = Vector(
//      Sphere(Vector3(0f, 0.2f, -1f), 0.2f, Lambertian(Vector3(0.8f, 0.3f, 0.3f))),
//      //Sphere(Vector3(0f, -100.5f, -1f), 100f, Lambertian(Vector3(0.8f, 0.8f, 0.0f))),
//      Sphere(Vector3(0f, -1000f, 0f), 1000f, Lambertian(Vector3(0.5f, 0.5f, 0.5f))),
//      Sphere(Vector3(1f, 0.2f, -1f), 0.2f, Metal(Vector3(0.8f, 0.6f, 0.2f), 0.01f)),
//      Sphere(Vector3(-1f, 0.2f, -1f), 0.2f, Metal(Vector3(0.8f, 0.8f, 0.8f), 0.2f)),
//      Sphere(Vector3(0f, 0.2f, -0.2f), 0.1f, Dielectric(Vector3(1f, 1f, 1f), 1.5f, 0.01f)),
//    )

  val xobj = Vector.range(-11,11)
  val yobj = Vector.range(-11,11)
  val objmap = for
    {a <- xobj
     b <- yobj} yield (a,b)

  val rand_objects = objmap.map( coord => random_obj(coord._1, coord._2) ) filter(_.isRight) map (_.right.get)
  val other_objects = Vector(
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

  // Jankiest parallelization
  val pixels1 = Future{draw(canvas, camera, objects, subsampling = samples, max_depth = max_bounces)}
  val pixels2 = Future{draw(canvas, camera, objects, subsampling = samples, max_depth = max_bounces)}
  val pixels3 = Future{draw(canvas, camera, objects, subsampling = samples, max_depth = max_bounces)}
  val pixels4 = Future{draw(canvas, camera, objects, subsampling = samples, max_depth = max_bounces)}
  val final_pixels1 :Vector[Vector3] = Await.result(pixels1, Duration.Inf).zip(Await.result(pixels2, Duration.Inf)).map { case (x, y) => ((x + y) * 0.5f) }
  val final_pixels2 :Vector[Vector3] = Await.result(pixels3, Duration.Inf).zip(Await.result(pixels4, Duration.Inf)).map { case (x, y) => ((x + y) * 0.5f) }
  val final_pixels :Vector [Vector3] = final_pixels1.zip(final_pixels2).map { case (x, y) => ((x + y) * 0.5f).elementwise_pow(gamma_corr) * bit_depth }
//  val final_pixels = draw(canvas, camera, objects, subsampling = 200, max_depth = 50).map(_.elementwise_pow(gamma_corr) * bit_depth)
  for (vec <- final_pixels) {writer.write(vec.intPrint) }

  writer.close()
  println((System.nanoTime - t1) / 1e9d + " seconds")
  }


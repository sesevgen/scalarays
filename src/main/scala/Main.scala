import java.io.File
import java.io.PrintWriter
import scala.math.sqrt

object Main extends App {

  def sky_color(r : ray): vector3 = {
    val unit_direction = r.direction.normalized
    val t:Float = 0.5f * (unit_direction.y + 1.0f)
    vector3(1.0f, 1.0f, 1.0f) * (1.0f-t) + vector3(0.5f, 0.7f, 1.0f) * t
  }

  def hit_sphere(center: vector3, radius: Float, r: ray): Float = {
    val oc = r.origin - center
    val a = r.direction.squared_length
    val b = 2.0f * (oc dot r.direction)
    val c = oc.squared_length - radius * radius
    val discriminant = b * b - 4 * a * c
    if (discriminant < 0) {-1.0f}
    else
      {(-b -scala.math.sqrt(discriminant))  / (2.0f*a)}.toFloat
  }

  def color(r: ray): vector3 = {
    val t = hit_sphere(vector3(0,0,-1), 0.5f, r)
    if ( t < 0) {sky_color(r)}
    else {
      val N = (r.point_at_parameter(t) - vector3(0,0,-1)).normalized
      vector3(N.x+1, N.y+1, N.z+1) * 0.5f
    }
  }

  val nx = 1000
  val ny = 500

  val lower_left_corner = vector3(-2, -1, -1)
  val horizontal = vector3(4, 0, 0)
  val vertical = vector3(0, 2, 0)
  val origin = vector3(0, 0, 0)

  val yrange = List.range(ny-1, -1, -1)
  val xrange = List.range(0, nx, 1)

  def processPixel(y: Int, x: Int) : String = {
    val u : Float = x.toFloat / nx.toFloat
    val v : Float = y.toFloat / ny.toFloat
    val r = ray(origin, lower_left_corner + horizontal * u + vertical * v)
    val col = color(r)
    (col * 255.99f).intPrint
  }

  val writer = new PrintWriter(new File("Test2.ppm"))
  writer.write("P3\n" + nx + " " + ny + "\n255\n")
  for {
    y <- yrange
    x <- xrange
  }
    yield writer.write(processPixel(y, x))
  writer.close()
}


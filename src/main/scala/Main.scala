import java.io.File
import java.io.PrintWriter

object Main extends App {
    
//  def rainbow(boundx: Int, boundy: Int, x: Int, y: Int) = {
//  val r: Double = x.asInstanceOf[Double] / boundx.asInstanceOf[Double]
//  val g: Double = y.asInstanceOf[Double] / boundy.asInstanceOf[Double]
//  val b: Double = 0.2
//
//  val ir: Int = (255.99*r).asInstanceOf[Int]
//  val ig: Int = (255.99*g).asInstanceOf[Int]
//  val ib: Int = (255.99*b).asInstanceOf[Int]
//
//  ir + " " + ig + " " + ib
//  }
//
//  val writer = new PrintWriter(new File("Test.ppm"))
//  val boundsx = 200
//  val boundsy = 100
//  writer.write("P3\n" + boundsx + " " + boundsy + "\n255\n")
//
//  def writeRainbowToFile(output: PrintWriter,
//                         boundsx: Int,
//                         boundsy: Int,
//                         startx: Int,
//                         starty: Int): Unit = {
//    if (starty >= 0) {
//      //println(rainbow(boundsx, boundsy, startx, starty))
//      output.write(rainbow(boundsx, boundsy, startx, starty)+"\n")
//      if (startx == boundsx-1) {
//        writeRainbowToFile(output, boundsx, boundsy, 0, starty-1)
//      }
//      else writeRainbowToFile(output, boundsx, boundsy, startx+1, starty)
//    }
//  }
//  writeRainbowToFile(writer, boundsx, boundsy, 0, 99)
//  writer.close()

  def sky_color(r : ray): vector3 = {
    val unit_direction = r.direction.normalized
    val t = 0.5 * (unit_direction.y + 1.0)
    vector3(1.0, 1.0, 1.0) * (1.0-t) + vector3(0.5, 0.7, 1.0) * t
  }

  val nx = 200
  val ny = 100

  val lower_left_corner = vector3(-2.0, -1.0, -1.0)
  val horizontal = vector3(4.0, 0.0, 0.0)
  val vertical = vector3(0.0, 2.0, 0.0)
  val origin = vector3(0.0, 0.0, 0.0)

  val yrange = List.range(ny-1, -1, -1)
  val xrange = List.range(0, nx, 1)

  //val pixels = yrange flatMap(x => xrange map (y => (x,y)))
  //val adjustedPixels = pixels.map { case (y, x) => (y.toFloat/ny, x.toFloat/nx) }

  def processPixel(y: Int, x: Int) : String = {
    val u : Float = x.toFloat / nx.toFloat
    val v : Float = y.toFloat / ny.toFloat
    val r = ray(origin, lower_left_corner + horizontal * u + vertical * v)
    val col = sky_color(r)
    (col * 255.99).intPrint
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


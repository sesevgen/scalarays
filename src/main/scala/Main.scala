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

  val writer = new PrintWriter(new File("Test2.ppm"))
  val nx = 200
  val ny = 100

  writer.write("P3\n" + nx + " " + ny + "\n255\n")
  val lower_left_corner = vector3(-2.0, -1.0, -1.0)
  val horizontal = vector3(4.0, 0.0, 0.0)
  val vertical = vector3(0.0, 2.0, 0.0)
  val origin = vector3(0.0, 0.0, 0.0)

  // Non functional for now, until I know what I'm doing better
  val yrange = List.range(ny-1, -1, -1)
  val xrange = List.range(0, nx, 1)
  println(yrange)
  println(xrange)
  for(j <- yrange){
    for(i <- xrange){
      val u : Float = i.toFloat / nx.toFloat
      val v : Float = j.toFloat / ny.toFloat
      val r = ray(origin, lower_left_corner + horizontal * u + vertical * v)
      val col = sky_color(r)
      val ir : Int = (255.99 * col.x).toInt
      val ig : Int = (255.99 * col.y).toInt
      val ib : Int = (255.99 * col.z).toInt

      writer.write(ir + " " + ig + " " + ib + "\n")
    }
  }
  writer.close()
}


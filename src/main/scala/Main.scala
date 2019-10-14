import java.io.File
import java.io.PrintWriter

object Main extends App {
    
  def rainbow(boundx: Int, boundy: Int, x: Int, y: Int) = {
  val r: Double = x.asInstanceOf[Double] / boundx.asInstanceOf[Double]
  val g: Double = y.asInstanceOf[Double] / boundy.asInstanceOf[Double]
  val b: Double = 0.2
  
  val ir: Int = (255.99*r).asInstanceOf[Int]
  val ig: Int = (255.99*g).asInstanceOf[Int]
  val ib: Int = (255.99*b).asInstanceOf[Int]
  
  ir + " " + ig + " " + ib
  }
  
  val writer = new PrintWriter(new File("Test.ppm"))
  val boundsx = 200
  val boundsy = 100
  writer.write("P3\n" + boundsx + " " + boundsy + "\n255\n")
  
  def writeRainbowToFile(output: PrintWriter, 
                         boundsx: Int,
                         boundsy: Int,
                         startx: Int,
                         starty: Int): Unit = {
    if (starty >= 0) {
      //println(rainbow(boundsx, boundsy, startx, starty))
      output.write(rainbow(boundsx, boundsy, startx, starty)+"\n")
      if (startx == boundsx-1) {
        writeRainbowToFile(output, boundsx, boundsy, 0, starty-1)
      }
      else writeRainbowToFile(output, boundsx, boundsy, startx+1, starty)
    }
  }
  writeRainbowToFile(writer, boundsx, boundsy, 0, 99)
  writer.close()
}


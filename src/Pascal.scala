

object Pascal extends App {
  def pascal(c: Int, r: Int): Int = {
    if (r==0){
      if (c==0) 1 else 0
    }
    else pascal(c-1, r-1)+pascal(c, r-1)
  }
  println(pascal(2,4))
}
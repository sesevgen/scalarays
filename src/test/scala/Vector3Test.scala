import org.scalatest.FlatSpec

class Vector3Test extends FlatSpec {
  val a: Vector3 = Vector3(1,2,3)
  val b: Vector3 = Vector3(4,5,6)

  "Two vectors" should "add" in {
    assert(a + b == Vector3(5, 7, 9))
  }

  "Two vectors" should "subtract" in {
    assert(b - a == Vector3(3, 3, 3))
  }

  "Two vectors" should "element-wise multiply" in {
    assert(a * b == Vector3(4, 10, 18))
  }

  "Two vectors" should "element-wise divide" in {
    assert(a / b == Vector3(1.0f/4, 2.0f/5, 3.0f/6))
  }

  "A scalar" should "multiply a vector" in {
    assert(a * 2 == Vector3(2,4,6))
  }

  "A scalar" should "divide a vector" in {
    assert(a / 2 == Vector3(1.0f/2, 2.0f/2, 3.0f/2))
  }

  "A vector" should "have a length" in {
    assert(a.length == scala.math.sqrt(scala.math.pow(1,2) + scala.math.pow(2,2) + scala.math.pow(3,2)).toFloat)
  }

  "Vector" should "flip signs" in {
    assert(-a == Vector3(-1, -2, -3))
  }

  "Two vectors" should "dot product" in {
    assert((a dot b) == 4 + 10 + 18)
  }

  "Two vectors" should "cross product" in {
    assert((a cross b) == Vector3(-3, 6, -3))
  }
}
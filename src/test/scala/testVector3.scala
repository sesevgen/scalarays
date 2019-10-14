object testVector3 extends App {
  val a = vector3(1,2,3)
  val b = vector3(4,5,6)

  assert(a + b == vector3(5,7,9))
  assert(b - a == vector3(3,3,3))
  
  assert(-a == vector3(-1,-2,-3))
  
  assert((a dot b) == 4 + 10 + 18)
  
  assert((a cross b) == vector3(-3, 6, -3))
}
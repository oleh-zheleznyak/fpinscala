package Excersises

object Excersise3 {
  def partial1[A,B,C](a: A, f: (A,B) => C): B => C =
  {
    val g = (b:B)=>f(a,b)
    g
  }
  
  def pow(x:Int, y:Int):Int = Math.pow(y,x).asInstanceOf[Int]
  
  def sqr = partial1[Int,Int,Int](2,pow)
}
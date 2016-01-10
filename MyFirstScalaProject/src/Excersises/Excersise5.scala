package Excersises

object Excersise5 {
  def uncurry[A,B,C](f: A => B => C): (A, B) => C = {
    val g = (a:A,b:B)=>f(a)(b)
    g
  }
}
package Excersises

object Excersise4 {
  def curry[A,B,C](f: (A, B) => C): A => (B => C) = {
    val g = (a:A)=>Excersise3.partial1(a, f)
    g
  }
}
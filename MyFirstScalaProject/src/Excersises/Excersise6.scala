package Excersises

object Excersise6 {
  def compose[A,B,C](f: B => C, g: A => B): A => C = {
    val h = (a:A)=>f(g(a))
    h
    }
}
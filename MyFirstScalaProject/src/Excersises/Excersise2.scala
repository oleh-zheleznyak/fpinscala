package Excersises

object Excersise2 {
  
  def isSorted[@specialized A](a : Array[A], f : (A,A)=>Boolean) : Boolean =
  {
    def g = (i:Int)=>f(a(i),a(i+1))
    
    @annotation.tailrec
    def go[@specialized A](a:Array[A], f:(A,A)=>Boolean, i:Int, b:Boolean):Boolean=
    {
      if (i+1<=a.length-1)
      {
        if (g(i)==b) go(a,f,i+1,b)
        else false
      }
      else true
    }
    go(a,f,0, g(0))
  }
  
  
}
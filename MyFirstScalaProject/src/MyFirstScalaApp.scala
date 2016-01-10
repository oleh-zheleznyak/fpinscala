// single line comment
/* multiline comment */
/** documentation comment */
object MyModule  {
 
  def abs(n:Int) : Int = 
    if (n<0) -n
    else n
  
  private def formatResult(name:String, x: Int, f:Int=>Int) = {
      val msg = "The %s value of %d is %d"
      msg.format(name,x, f(x))
    }
  
  def main(args:Array[String]) : Unit =
  {
    //println("%b".format(Excersises.Excersise2.isSorted[Int](Array(7,4,10,1), (x:Int,y:Int)=>x<y)));
    //println(formatResult("factorial",5,factorial))
    //println("%d".format(Excersises.Excersise3.sqr(3)));
    
    val a = Nil
    val b = LinkedList(3.0, 0.0, 2.0)
    val c = LinkedList(1,2,3)
    
    println(LinkedList.increment(c))
  }
    
  def factorial(n:Int) : Int =
  {
    @annotation.tailrec
    def go(n:Int, acc:Int) : Int =
     if (n<=0) acc
     else go(n-1,n*acc)
    
    go(n,1)
  }
  
 
  
}
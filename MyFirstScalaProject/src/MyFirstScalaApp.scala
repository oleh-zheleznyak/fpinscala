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
    val b = LinkedList(1,3,5)
    val c = LinkedList(2,4,6,8,10)
    val d = LinkedList(4,8,10)
    val dd = LinkedList(b,c,d)
    
    //println(LinkedList.hasSubsequence(c, d))
    
    val t = new Branch(new Leaf(2), new Leaf(3))
    println(Tree.depth(t))
    
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
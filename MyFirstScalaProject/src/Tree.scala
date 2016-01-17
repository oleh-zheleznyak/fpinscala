

sealed trait Tree[+A] 
case class Leaf[A](value:A) extends Tree[A]
case class Branch[A](left:Tree[A], right:Tree[A]) extends Tree[A]

object Tree
{
  def size[A](t:Tree[A]):Int = {
    
    def go[A](t:Tree[A]):Int = t match {
      case Leaf(_)=> 1
      case Branch(x,y)=>go(x)+go(y)+1
    }
    
    go(t)
  }
  
    def max[A](t:Tree[A])(f:(A,A)=>A):A = {
    
    def go[A](t:Tree[A])(f:(A,A)=>A):A = t match {
      case Branch(Leaf(x), Leaf(y))=>f(x,y)
      case Branch(x,y)=>f(go(x)(f),go(y)(f))
    }
    
    go(t)(f)
  }
    
    def depth[A](t:Tree[A]):Int = {
      def go[A](t:Tree[A], d:Int):Int = t match {
        case Leaf(_)=>d+1
        case Branch(x,y)=> go(x,d+1) max go(y,d+1)
      }
      
      go(t,0)
    }
  
 
}

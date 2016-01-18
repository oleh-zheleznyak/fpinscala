

sealed trait Tree[+A] 
case class Leaf[A](value:A) extends Tree[A]
case class Branch[A](left:Tree[A], right:Tree[A]) extends Tree[A]

object Tree
{
  def size[A](t:Tree[A]):Int =  t match {
      case null=>0
      case Leaf(_)=> 1
      case Branch(x,y)=>size(x)+size(y)+1
  }
  
    def sizeWithFold[A](t:Tree[A]):Int = fold[A,Int](t,0)((a,b)=>b+1)
  
    def max[A](t:Tree[A])(f:(A,A)=>A):A =  t match { 
      case Branch(Leaf(x), Leaf(y))=>f(x,y)
      case Branch(x,y)=>f(max(x)(f),max(y)(f))
  }
    
    def depth[A](t:Tree[A]):Int = {
      def go[A](t:Tree[A], d:Int):Int = t match {
        case null=>0
        case Leaf(_)=>d+1
        case Branch(x,y)=> go(x,d+1) max go(y,d+1)
      }
      
      go(t,0)
    }
    
    def map[A,B](t:Tree[A])(f:A=>B):Tree[B] = t match {
      case null=>null
      case Leaf(a)=>Leaf(f(a))
      case Branch(x,y)=>Branch(map(x)(f),map(y)(f))
    }
  
    def fold[A,B](t:Tree[A], b:B)(f:(A,B)=>B):B = t match {
      case Leaf(a)=>f(a,b)
      case Branch(x,y)=>fold(x,fold(y,b)(f))(f)
    }
    
 
}

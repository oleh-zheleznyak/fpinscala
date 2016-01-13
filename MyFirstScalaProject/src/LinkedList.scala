

sealed trait LinkedList[+A] 

case object Nil extends LinkedList[Nothing]
case class Cons[+A](head:A, tail:LinkedList[A]) extends LinkedList[A]

object LinkedList {
  def apply[A](as:A*):LinkedList[A]= {
    if (as.isEmpty) Nil
    else Cons(as.head, apply(as.tail : _*))
  }
  
  def sum(ints:LinkedList[Int]) : Int = foldRight(ints,0)(_+_)
  
    // Chapter 3, Excersise 7
  def product(dbls:LinkedList[Double]) : Double = foldRightWithBreak(dbls, 1.0)(0.0,0.0)(_*_)
  
  def even(ints:LinkedList[Int]) : Boolean = ints match {
    case Nil => true
    case Cons(x,xs) => (x % 2 ==0) && even(xs)
  }
  
  def tail[A](as:LinkedList[A]):LinkedList[A] = as match {
    case Nil => Nil // could also throw exception ?
    case Cons(x,xs)=>xs
  }
  
  def drop[A](as: LinkedList[A], n: Int): LinkedList[A] = n match {
    case 0 => as
    case n => drop(tail(as),n-1)
  }
  
  def dropWhile[A](as: LinkedList[A])(f: A => Boolean): LinkedList[A] = as match {
    case Nil => Nil
    case Cons(x,xs) => f(x) match
      {
      case true => dropWhile(xs)(f)
      case false => Cons(x,xs)
        
      }
  }
  
  def setHead[A](as: LinkedList[A], a:A):LinkedList[A] = as match {
    case Nil => Cons(a,Nil)
    case Cons(x,xs) => Cons(a, Cons(x,xs))
  }
  
  // Chapter 3, Excersise 6
  def cutTail[A](as:LinkedList[A]):LinkedList[A] =  {
    
    def build[A](from:LinkedList[A], to:LinkedList[A]):LinkedList[A] = from match {
      case Nil => Nil
      case Cons(a,Nil)=>to
      case Cons(a,as)=>
        {
          val b = build(as, to)
          Cons(a,b)
        }
    }
  
    build(as,Nil)
  }
  
  def foldRight[A,B](as:LinkedList[A], z:B)(f:(A,B)=>B):B = as match {
    case Nil => z
    case Cons(x,xs) => f(x,foldRight(xs,z)(f))
  }
  
  // Chapter 3, Excersise 10
  def foldLeft[A,B](l: LinkedList[A], z: B)(f: (B, A) => B): B = {
    @annotation.tailrec
    def go[A,B](l: LinkedList[A], z: B)(f: (B, A) => B): B = l match {
      case Nil => z
      case Cons(x,Nil)=> f(z,x)
      case Cons(x,xs)=> go(xs,f(z,x))(f)
    }
    
    go(l,z)(f)
  }
  
  // Chapter 3, Excersise 11
  def sumLeft(l:LinkedList[Int]):Int = foldLeft(l,0)(_+_)
  
  // Chapter 3, Excersise 11
  def productLeft(l:LinkedList[Int]):Int = foldLeft(l,1)(_*_)
  
  // Chapter 3, Excersise 11
  def lengthLeft[A](l:LinkedList[A]):Int = foldLeft(l,0)((b,a)=>(b+1))
  
  // Chapter 3, Excersise 7
  def foldRightWithBreak[A,B](as:LinkedList[A], z:B)(a:A, b:B)(f:(A,B)=>B):B = as match {
    case Nil => z
    case Cons(x,xs) => {
      if (x==a) { println("break"); b }
      else f(x,foldRightWithBreak(xs,z)(a,b)(f))
    }
  }
  
  // Chapter 3, Excersise 8
  def copy[A](as:LinkedList[A]) : LinkedList[A] = foldRight(as,Nil:LinkedList[A])(Cons(_,_))
  
  // Chapter 3, Excersise 9
  def length[A](as: LinkedList[A]): Int = foldRight(as, 0)((a,b)=>(b+1))
  
  // Chapter 3, Excersise 12
  def reverseManual[A](l : LinkedList[A]):LinkedList[A] = {
    @annotation.tailrec
    def go[A](from : LinkedList[A], to:LinkedList[A]):LinkedList[A] = from match {
      case Nil=>to
      case Cons(x,xs)=>go(xs,Cons(x,to))
    }
    go(l, Nil)
  }
  
  // Chapter 3, Excersise 12
  def reverse[A](l:LinkedList[A]):LinkedList[A] = foldLeft[A,LinkedList[A]](l,Nil:LinkedList[A])((as,a)=>Cons(a,as))
  
  // Chapter 3, Excersise 14
  def append[A](l:LinkedList[A], a:A):LinkedList[A] = foldRight[A,LinkedList[A]](l, Cons(a,Nil))(Cons(_,_))
  
  // Chapter 3, Excersise 14
  def append[A](l:LinkedList[A], m:LinkedList[A]):LinkedList[A] = foldRight[A,LinkedList[A]](l,m)((a,as)=>Cons(a,as))
  
  // Chapter 3, Excersise 15
  // TODO: this probably does not have linear time - check and fix
  def flatten[A](lls:LinkedList[LinkedList[A]]):LinkedList[A] = foldRight[LinkedList[A],LinkedList[A]](lls,Nil:LinkedList[A])(append)
  
  //Chapter 3, Excersise 16
  def increment(l:LinkedList[Int]) : LinkedList[Int] = foldRight(l,Nil:LinkedList[Int])((i,is)=>Cons(i+1,is))
  
    //Chapter 3, Excersise 17
  def toString[A](l:LinkedList[A]) : LinkedList[String] = foldRight(l,Nil:LinkedList[String])((i,is)=>Cons(i.toString(),is))
  
  //Chapter 3, Excersise 18
  def map[A,B](l: LinkedList[A])(f: A => B): LinkedList[B] = foldRight(l, Nil:LinkedList[B])((i,is)=>Cons(f(i),is))
  
  //Chapter 3, Excersise 19
  def filter[A](l:LinkedList[A])(f: A => Boolean):LinkedList[A] = {
    //@annotation.tailrec
    def go[A](from:LinkedList[A])(f: A => Boolean):LinkedList[A] = from match {
      case Nil=>Nil
      case Cons(x,xs)=> f(x) match {
        case true => Cons(x,go(xs)(f))
        case false => go(xs)(f)
      }
    }
    go(l)(f)
  }
  
  //Chapter 3, Excersise 20
  def flatMap[A,B](l: LinkedList[A])(f: A => LinkedList[B]): LinkedList[B] = flatten[B](( map[A,LinkedList[B]](l)(f)))
  
  //Chapter 3, Excersise 21
  def filterUsingFlatMap[A](l:LinkedList[A])(f: A => Boolean):LinkedList[A] = flatMap[A,A](l)(a=> { if(f(a)) Cons(a,Nil) else Nil })
  
  //Chapter 3, Excersise 23
  def add[A](a1:LinkedList[A], a2:LinkedList[A])(f:(A,A)=>A) : LinkedList[A] = {
    
    def go[A](a1:LinkedList[A], a2:LinkedList[A],  f:(A,A)=>A) : LinkedList[A] = (a1,a2) match {
      case (Cons(x,Nil),Cons(y,Nil)) => Cons(f(x,y),Nil)  
      case (Cons(x,xs),Cons(y,ys)) => Cons(f(x,y),go(xs,ys,f))
      case _ => Nil
    }
    
    go(a1,a2,f)
  }
  
   //Chapter 3, Excersise 24
  def hasSubsequence[A](l: List[A], sub: List[A]): Boolean = { false }
  }

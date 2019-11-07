sealed trait List[+A] // `List` data type, parameterized on a type, `A`
case object Nil extends List[Nothing] // A `List` data constructor representing the empty list
/* Another data constructor, representing nonempty lists. Note that `tail` is another `List[A]`,
which may be `Nil` or another `Cons`.
 */
case class Cons[+A](head: A, tail: List[A]) extends List[A]

object List { // `List` companion object. Contains functions for creating and working with lists.
  def sum(ints: List[Int]): Int = ints match { // A function that uses pattern matching to add up a list of integers
    case Nil => 0 // The sum of the empty list is 0.
    case Cons(x,xs) => x + sum(xs) // The sum of a list starting with `x` is `x` plus the sum of the rest of the list.
  }

  def product(ds: List[Double]): Double = ds match {
    case Nil => 1.0
    case Cons(0.0, _) => 0.0
    case Cons(x,xs) => x * product(xs)
  }

  def apply[A](as: A*): List[A] = // Variadic function syntax
    if (as.isEmpty) Nil
    else Cons(as.head, apply(as.tail: _*))
	
	/*ex3.2*/
	def tail[A](ls: List[A]): List[A] = ls match{
		case Nil => sys.error("tail of empty list")
		case Cons(_, xs) => xs
	}
	
	/*ex3.3*/
	def setHead[A](ls: List[A], newHead: A): List[A] = 
		ls match{
			case Nil => sys.error("Empty list has no head.")
			case Cons(_, xs) => Cons(newHead, xs)
		}
	
	/*ex3.4*/
	def drop[A](ls: List[A], n: Int): List[A] =
		if (n <= 0) ls
		else ls match {
			case Nil => Nil
			case Cons(_, xs) => drop(xs, n-1)
		}
	
	/*ex3.5*/
	def dropWhile[A](ls: List[A])(f: A => Boolean): List[A] = 
		ls match{
			case Nil => Nil
			case Cons(h, t) => {
				if(!f(h)) ls
				else dropWhile(t)(f)
			}
		}
	
	/*ex3.6*/
	def init[A](l: List[A]): List[A] = 
		l match{
			case Nil => Nil
			case Cons(h, Nil) => Nil
			case Cons(h, t) => Cons(h, init(t))
		}
	
	def foldRight[A,B](l: List[A], z: B)(f: (A,B)=>B): B = 
		l match{
			case Nil => z
			case Cons(h,t) => f(h,foldRight(t,z)(f))
		}
		
	/*ex3.7*/
	def foldRightWithShort[A,B](l: List[A], z:B, shortVal:B)(f: (A,B)=>B, shortJudge: A=>Boolean) : B = 
		l match{
			case Nil => z
			case Cons(h,t) if shortJudge(h) => shortVal
			case Cons(h,t) => f(h, foldRightWithShort(t, z, shortVal)(f,shortJudge))
		}
	
	/*ex3.9*/
	def lengthVarFoldRight[A](l : List[A]): Int = 
		foldRight(l, 0)((_, len)=>len+1)
	
	/*ex3.10*/
	@annotation.tailrec
	def foldLeft[A,B](l : List[A], z: B)(f:(B,A)=>B): B = 
		l match{
			case Nil => z
			case Cons(h,t) => foldLeft(t, f(z,h))(f)
		}
	
	/*ex3.11*/
	def sumViaFoldLeft(ints: List[Int]): Int = 
		foldLeft(ints, 0)(_ + _)
		
	def productViaFoldLeft(ds: List[Double]): Double = 
		foldLeft(ds, 1.0)(_ * _)
		
	def lengthViaFoldLeft[A](l : List[A]): Int = 
		foldLeft(l, 0)((len,_) => len+1)
	
	/*ex3.12*/
	def reverse[A](l : List[A]): List[A] = 
		foldLeft(l,Nil: List[A])((t,h)=>Cons(h,t))
	
	/*ex3.14*/
	def appendViaFoldRight[A](l1: List[A], l2: List[A]): List[A] =
		foldRight(l1, l2)((h,t)=>Cons(h,t))
	
	/*ex3.16*/
	def addOne(l: List[Int]) : List[Int] = 
		l match{
			case Nil => Nil
			case Cons(h, t) => Cons(h+1, addOne(t))
		}
	
	/*ex3.17*/
	def tranDoubleToStr(l: List[Double]) : List[String] = 
		l match {
			case Nil => Nil
			case Cons(h, t) => Cons(h.toString, tranDoubleToStr(t))
		}
	
	/*ex3.18*/
	def map[A,B](l : List[A])(f: A=>B) : List[B] = 
		l match {
			case Nil => Nil
			case Cons(h, t) => Cons(f(h), map(t)(f))
		}
	
	/*ex3.19*/
	def filter[A](l : List[A])(f: A=>Boolean): List[A] = 
		
}
import Stream._

sealed trait Stream[+A]{
	def headOption: Option[A] = this match{
		case Empty => None
		case Cons(h, t) => Some(h())
	}
	
	/*ex5.1*/
	def toList: List[A] = {
		
		def go(s:Stream[A]): List[A] = s match {
			case Cons(h,t) => h()::go(t())
			case _ => Nil
		}
		go(this)
	}
	
	/*ex5.2*/
	def take(n : Int) : Stream[A] = this match {
		case Cons(h,t) if n > 1 => cons(h(),t().take(n-1))
		case Cons(h,_) if n == 1 => cons(h(), empty)
		case _ => empty
	}
	
	/*ex5.3*/
	def takeWhile(p: A => Boolean): Stream[A] = this match{
		case Cons(h,t) if(p(h())) => cons(h(), t().takeWhile(p))
		case _ => empty
	}
	
	def foldRight[B](z: => B)(f: (A, => B) => B): B = 
		this match{
			case Cons(h,t) => f(h(), t().foldRight(z)(f))
			case _ => z
		}
	
	def exists(p : A=>Boolean): Boolean = 
		foldRight(false)((a, b) => p(a) || b)
	
	def foldLeft[B](z: => B)(f: ( => B, A) => B): B = 
		this match{
			case Cons(h,t) => f(t().foldLeft(z)(f), h()) 
			case _ => z
		}
	/*ex5.4*/
	def forAll(p: A=>Boolean) : Boolean = 
		foldLeft(true)((b,a) => p(a) && b)
}
case object Empty extends Stream[Nothing]
case class Cons[+A](h: () => A, t: () => Stream[A]) extends Stream[A]

object Stream{
	def cons[A](hd: => A, tl: => Stream[A]): Stream[A] = {
		lazy val head = hd;
		lazy val tail = tl;
		
		Cons(() => head, () => tail)
	}
	
	def empty[A]: Stream[A] = Empty
	
	def apply[A](as: A*): Stream[A] = 
		if(as.isEmpty) empty else cons(as.head, apply(as.tail: _*))
	
	def test1(nz :  => Int) : () => Int = {
		lazy val lazyNz = nz
		() => lazyNz + 1;
	}

}
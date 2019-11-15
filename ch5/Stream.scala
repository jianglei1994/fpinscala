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
	
	/*ex5.5*/
	def takeWhileViaFoldRight(p : A => Boolean): Stream[A] = 
		foldRight(empty[A])( (a,b) => if(p(a)) cons(a, b) else empty )
	
	/*ex5.6*/
	def headOptionViaFoldRight(): Option[A] = 
		foldRight(None: Option[A])( (a,b) => Some(a) )
	
	/*ex5.7*/
	def map[B](f: A => B): Stream[B] = 
		foldRight(empty[B])((a,b) => cons(f(a),b))
	
	def filter(f: A=>Boolean): Stream[A] = 
		foldRight(empty[A])( (h,t) => if(f(h)) cons(h,t) else t)
	
	def flatMap[B](f: A => Stream[B]): Stream[B] = 
		foldRight(empty[B])( (h,t) => f(h).append(t) )
	
	def append[B>:A](s: => Stream[B]): Stream[B] =
		foldRight(s)( (h,t) => cons(h,t) )  
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
	
	/*5.8*/
	def constant[A](a:A):Stream[A] = 
		cons(a, constant(a))
	
	/*5.9*/
	def from(n: Int): Stream[Int] = 
		cons(n, from(n+1))
	
	/*5.10*/
	def fibs: Stream[Int] = {
		def go(f0: Int, f1: Int):Stream[Int] = {
			cons(f0, go(f1, f0+f1))
		}
		go(0,1)
	}
	
	/*ex5.11*/
	def unfold[A,S](z: S)(f: S=>Option[(A,S)]): Stream[A] = {
		f(z) match{
			case Some((a,s)) => cons(a, unfold(s)(f))
			case None => empty
		}
	}
	
	/*ex5.12*/
	def fromViaUnfold(n: Int): Stream[Int] = {
		unfold(n)( n => Some(n, n+1) )
	}
	
	def fibsViaUnfold: Stream[Int] = {
		unfold((0,1)){case (f0:Int,f1:Int) => Some(f0, (f1,f0+f1))}
	}
	
	def constantViaUnfold[A](a: A): Stream[A] = {
		unfold(a)( a => Some((a,a)))
	}
	
	def mapViaUnfold[A,B](f: A=>B):Stream[]
		

}
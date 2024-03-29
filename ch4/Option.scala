import scala.{Option => _, Either => _, _}

sealed trait Option[+A] {
  /*ex4.1*/
  def map[B](f: A => B): Option[B] = this match {
    case None => None
	case Some(a) => Some(f(a))
  }
  
  def getOrElse[B>:A](default: => B): B = this match {
	case None => default
	case Some(a) => a
  }
  
  def flatMap[B](f: A => Option[B]): Option[B] = this match{
	case None => None
	case Some(a) => f(a)
  }

  def flatMap1[B](f: A => Option[B]): Option[B] = 
	map(f).getOrElse(None)

  def orElse[B>:A](ob: => Option[B]): Option[B] = this match {
	case None => ob
	case _ => this
  }
  
  def orElse1[B>:A](ob: => Option[B]): Option[B] = 
	map(a => Some(a)).getOrElse(ob)


  def filter(f: A => Boolean): Option[A] = this match {
    case Some(a) if (f(a)) => this
	case _ => None
  }
  
  def filter1(f: A => Boolean): Option[A] = 
	flatMap(a => if(f(a)) Some(a) else None)
	
	
		
	
	
	def map2[A,B,C](oa: Option[A], ob:Option[B])(f: (A,B)=>C):Option[C] = 
		oa.flatMap(a=>ob.flatMap(b=>Option.Try(f(a,b))))
	
	
}
case class Some[+A](get: A) extends Option[A]
case object None extends Option[Nothing]

object Option{
def Try[A](a: => A):Option[A] = 
		try Some(a)
		catch {case e: Exception => None}

	/*ex4.2*/
	def mean(xs: Seq[Double]): Option[Double] = 
		if(xs.isEmpty) None
		else Some(xs.sum/ xs.length)
		
	def variance(xs: Seq[Double]): Option[Double] =
		mean(xs).flatMap(m => mean(xs.map((x=>math.pow(x-m,2)))))


	def sequence[A](a: List[Option[A]]): Option[List[A]] = 
		a match{
			case Nil => Some(Nil)
			case h::t => h.flatMap(hh => sequence(t).map(hh :: _))
		}
	
	def traverse[A,B](a: List[A])(f: A=> Option[B]):Option[List[B]] = 
		a match{
			case Nil => Some(Nil)
			case h::t => f(h).flatMap( hh => traverse(t)(f).map(hh :: _))
		}

}
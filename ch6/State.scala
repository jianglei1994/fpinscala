trait RNG {
  def nextInt: (Int, RNG) // Should generate a random `Int`. We'll later define other functions in terms of `nextInt`.
}

object RNG {
  // NB - this was called SimpleRNG in the book text

  case class SimpleRNG(seed: Long) extends RNG {
    def nextInt: (Int, RNG) = {
      val newSeed = (seed * 0x5DEECE66DL + 0xBL) & 0xFFFFFFFFFFFFL // `&` is bitwise AND. We use the current seed to generate a new seed.
      val nextRNG = SimpleRNG(newSeed) // The next state, which is an `RNG` instance created from the new seed.
      val n = (newSeed >>> 16).toInt // `>>>` is right binary shift with zero fill. The value `n` is our new pseudo-random integer.
      (n, nextRNG) // The return value is a tuple containing both a pseudo-random integer and the next `RNG` state.
    }	
  }
  
  /*ex6.1*/
  def nonNegativeInt(rng: RNG): (Int, RNG) = {
		val (i: Int, r: RNG) = rng.nextInt;
		return (if(i<0) -(i+1) else i,r)
	}
	
	/*ex6.2*/
	def double(rng: RNG): (Double, RNG) = {
		val (i: Int, r: RNG) = nonNegativeInt(rng)
		return (if(i==0) 0.0 else (i-1)*1.0/Int.MaxValue, r)
	}
	
	/*ex6.3*/
	def intDouble(rng: RNG): ((Int,Double), RNG) = {
		val (i: Int, r1: RNG) = rng.nextInt
		val (d: Double, r2: RNG) = double(r1)
		return ((i,d), r2)
	}
	
	/*ex6.4*/
	def ints(count: Int)(rng: RNG): (List[Int], RNG) = {
		if(count <= 0)
			(Nil, rng)
		else{
			val (i, r) = rng.nextInt
			val (intList, rr) = ints(count-1)(r)
			(i::intList, rr)
		}		
	}
	
	type Rand[+A] = RNG => (A, RNG)
	
	def unit[A](a: A): Rand[A] = rng => (a, rng)
	
	def map[A,B](s: Rand[A])(f: A=>B): Rand[B] = 
		rng => {
			val (a, r) = s(rng)
			(f(a), r)
		}
	
	def nonNegativeIntViaMap: Rand[Int] = map(r => r.nextInt)(i => if(i < 0) -(i+1) else i)
	
	def nonNeagtiveEven: Rand[Int] = 
		map(nonNegativeInt)(i => i - i % 2)
		
	/*ex6.5*/
	def doubleViaMap: Rand[Double] = map(nonNegativeInt)(i => if(i==0) 0.0 else (i-1)*1.0/Int.MaxValue)
	
	/*ex6.6*/
	def map2[A,B,C](ra: Rand[A], rb: Rand[B])(f: (A,B)=>C): Rand[C] = {
		rng => {
			val (a, rngA) = ra(rng)
			val (b, rngB) = rb(rngA)
			(f(a,b), rngB)
		}
	}
	
	def both[A,B](ra: Rand[A], rb: Rand[B]): Rand[(A,B)] = map2(ra,rb)((_,_))
	
	def randIntDouble: Rand[(Int, Double)] = both(rng => rng.nextInt, double)
	
	def sequence[A](fs: List[Rand[A]]): Rand[List[A]] = {
		rng => {
			fs match{
				case Nil => (Nil, rng)
				case h::t => {
					val (v1, v2) = sequence(t)(rng)
					val (v3, v4) = h(v2)
					(v3::v1, v4)
				}
			}
		}
	}
	
	def flatMap[A,B](f: Rand[A])(g: A=>Rand[B]): Rand[B] = 
		rng => {
			val (v1, r1) = f(rng)
			g(v1)(r1)
		}
	
	def nonNegativeLessThan(n: Int): Rand[Int] = 
		flatMap(nonNegativeInt){ i =>
			val mod = i % n
			if (i + (n-1) - mod >= 0) unit(mod) else nonNegativeLessThan(n)
		}
	
	def mapViaFlatMap[A,B](s: Rand[A])(f: A=>B): Rand[B] = 
		flatMap(s){a => unit(f(a))}
	
	def map2ViaFlatMap[A,B,C](ra: Rand[A], rb: Rand[B])(f: (A,B)=>C): Rand[C] = 
		flatMap(ra){ a => (flatMap(rb){b => unit(f(a,b))}) }
	
 }
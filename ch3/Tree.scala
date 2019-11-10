sealed trait Tree[+A]
case class Leaf[A](value : A) extends Tree[A]
case class Branch[A](left: Tree[A], right: Tree[A]) extends Tree[A]

object Tree{
	
	/*ex3.25*/
	def size[A](tree: Tree[A]): Int = 
		tree match{
			case Leaf(v) => 1
			case Branch(l, r) => size(l) + size(r) + 1
		}
	
	/*ex3.26*/
	def maximum(tree : Tree[Int]) : Int = 
		tree match{
			case Leaf(v) => v
			case Branch(l, r) => maximum(l) max maximum(r)
		}
	
	/*ex3.27*/
	def depth[A](tree : Tree[A]) : Int = 
		tree match{
			case Leaf(v) => 1
			case Branch(l, r) => (depth(l)+1) max (depth(r)+1)
		}
	
	/*ex3.28*/
	def map[A,B](tree : Tree[A])(f: A=>B) : Tree[B] = 
		tree match{
			case Leaf(v) => Leaf(f(v))
			case Branch(l,r) => Branch(map(l)(f),map(r)(f))
		}
	
	/*ex3.29*/
	def fold[A,B](tree : Tree[A])(lf : A=>B, bf : (B,B)=>B) : B = 
		tree match{
			case Leaf(v) => lf(v)
			case Branch(l,r) => bf(fold(l)(lf,bf), fold(r)(lf,bf))
		}
		
	def sizeViaFold[A](tree : Tree[A]): Int = 
		fold(tree)((v)=>1, (l:Int, r:Int) => l+r+1)
		
	def maximumViaFold(tree : Tree[Int]): Int = 
		fold(tree)((v)=>v, (l:Int, r:Int) => l max r)
	
	def depthViaFold[A](tree : Tree[A]): Int = 
		fold(tree)((v)=>1, (l:Int, r:Int) => (l max r)+1)
}
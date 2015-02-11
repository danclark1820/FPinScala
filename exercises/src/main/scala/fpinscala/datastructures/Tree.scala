package fpinscala.datastructures

sealed trait Tree[+A]
case class Leaf[A](value: A) extends Tree[A]
case class Branch[A](left: Tree[A], right: Tree[A]) extends Tree[A]



object Tree {

  val t = new Branch(new Branch(new Leaf(0), new Leaf(1)), new Branch(new Leaf(2), new Leaf(3)))

  def size[A](t:Tree[A]):Int = t match {
    case Leaf(_) => 1
    case Branch(l, r) => 1 + size(l) + size(r)
  }

  def maximum(t:Tree[Int]):Int = t match {
    case Leaf(b) => b
    case Branch(l,r) => maximum(l) max maximum(r)
  }

  def depth[A](t:Tree[A]):Int = t match {
    case Leaf(_) => 0
    case Branch(l, r) => 1 + (depth(l) max depth(r))
  }
  

}

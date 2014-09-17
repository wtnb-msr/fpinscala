package fpinscala.datastructures

sealed trait Tree[+A]
case class Leaf[A](value: A) extends Tree[A]
case class Branch[A](left: Tree[A], right: Tree[A]) extends Tree[A]

object Tree {

  // ex. 3.25
  def count[A](tree: Tree[A]): Int = {
    tree match {
      case Leaf(v) => 1
      case Branch(l, r) => 1 + count(l) + count(r)
    }
  }

  // ex. 3.25 ?
  def size[A](tree: Tree[A]): Int = {
    tree match {
      case Leaf(v) => 1
      case Branch(l, r) => 1 + size(l) + size(r)
    }
  }

  // ex. 3.26
  def maximum(tree: Tree[Int]): Int = {
    tree match {
      case Leaf(v) => v
      case Branch(l, r) => maximum(l) max maximum(r)
    }
  }

  // ex. 3.27
  def depth[A](tree: Tree[A]): Int = {
    tree match {
      case Leaf(v) => 1
      case Branch(l, r) => (depth(l) max depth(r)) + 1
    }
  }

  // ex. 3.28
  def map[A, B](tree: Tree[A])(f: A => B): Tree[B] = {
    tree match {
      case Leaf(v) => Leaf(f(v))
      case Branch(l, r) => Branch(map(l)(f), map(r)(f))
    }
  }

  // ex. 3.29
  def foldRight[A, B](tree: Tree[A])(z: B)(f: (A, B) => B): B = {
    tree match {
      case Leaf(v) => f(v, z)
      case Branch(l, r) => foldRight(l)(foldRight(r)(z)(f))(f)
    }
  }

  def foldLeft[A, B](tree: Tree[A])(z: B)(f: (B, A) => B): B = {
    tree match {
      case Leaf(v) => f(z, v)
      case Branch(l, r) => foldLeft(r)(foldLeft(l)(z)(f))(f)
    }
  }

  def sizeViaFold[A](tree: Tree[A]): Int = {
    foldLeft(tree)(0)((b, _) => b + 1)
  }

  def maximumViaFold(tree: Tree[Int]): Int = {
    foldLeft(tree)(0)((b, a) => b max a)
  }

  def depthViaFold[A](tree: Tree[A]): Int = {
//    foldLeft(tree)(0)((b, a) => )
    1
  }
}
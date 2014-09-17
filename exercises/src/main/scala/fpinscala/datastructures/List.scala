package fpinscala.datastructures

sealed trait List[+A] // `List` data type, parameterized on a type, `A`
case object Nil extends List[Nothing] // A `List` data constructor representing the empty list
case class Cons[+A](head: A, tail: List[A]) extends List[A] // Another data constructor, representing nonempty lists. Note that `tail` is another `List[A]`, which may be `Nil` or another `Cons`.

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

  val x = List(1,2,3,4,5) match {
    case Cons(x, Cons(2, Cons(4, _))) => x
    case Nil => 42 
    case Cons(x, Cons(y, Cons(3, Cons(4, _)))) => x + y
    case Cons(h, t) => h + sum(t)
    case _ => 101 
  }

  def append[A](a1: List[A], a2: List[A]): List[A] =
    a1 match {
      case Nil => a2
      case Cons(h,t) => Cons(h, append(t, a2))
    }

  def foldRight[A,B](as: List[A], z: B)(f: (A, B) => B): B = // Utility functions
    as match {
      case Nil => z
      case Cons(x, xs) => f(x, foldRight(xs, z)(f))
    }
  
  def sum2(ns: List[Int]) = 
    foldRight(ns, 0)((x,y) => x + y)

  def product2(ns: List[Double]) = 
    foldRight(ns, 1.0)(_ * _) // `_ * _` is more concise notation for `(x,y) => x * y`; see sidebar

  // ex. 2
  def tail[A](l: List[A]): List[A] = {
    l match {
      case Nil => Nil
      case Cons(x, xs) => xs
    }
  }

  // ex. 3
  def setHead[A](l: List[A], h: A): List[A] = {
    l match {
      case Nil => Cons(h, Nil)
      case Cons(x, xs) => Cons(h, xs)
    }
  }

  // ex. 4
  def drop[A](l: List[A], n: Int): List[A] = {
    l match {
      case _ if (n <= 0) => l
      case Cons(x, xs) => drop(xs, n - 1)
      case Nil => Nil
    }
  }

  // ex. 5
  def dropWhile[A](l: List[A], f: A => Boolean): List[A] = {
    l match {
      case Nil => Nil
      case Cons(x, xs) if f(x) => dropWhile(xs, f)
      case _ => l
    }
  }

  // ex. 6
  def init[A](l: List[A]): List[A] = {
    l match {
      case Nil => Nil
      case Cons(x, xs) if (xs == Nil) => Nil
      case Cons(x, xs) => Cons(x, init(xs))
    }
  }

  // ex. 9
  def length[A](l: List[A]): Int = {
    foldRight(l, 0)( (a,b) => 1 + b)
  }

  // ex. 10
  @annotation.tailrec
  def foldLeft[A,B](l: List[A], z: B)(f: (B, A) => B): B = {
    l match {
      case Nil => z
      case Cons(x, xs) => foldLeft(xs, f(z, x))(f)
    }
  }

  // ex. 11
  def sumLeft(l: List[Int]): Int = {
    foldLeft(l, 0)(_ + _)
  }
  def productLeft(l: List[Double]): Double = {
    foldLeft(l, 1.0)(_ * _)
  }
  def lengthLeft[A](l: List[A]): Int = {
    foldLeft(l, 0)((b, a) => b + 1)
  }

  // ex. 12
  def reverse(l: List[Int]): List[Int] = {
    foldLeft(l, Nil: List[Int])( (b, a) => Cons(a, b) )
  }

  // ex. 13
  def foldRightWithLeft[A,B](as: List[A], z: B)(f: (A, B) => B): B = {
    foldLeft( foldLeft(as, Nil:List[A])( (b, a) => Cons(a, b)) , z)((b, a) => f(a, b))
  }

  // ex. 14
  def appendRight[A](a1: List[A], a2: List[A]): List[A] = {
    foldRight(a1, a2)(Cons(_, _))
  }

  // ex. 15
  def concatenate[A](l: List[List[A]]): List[A] = {
    foldRight(l, Nil: List[A])( (a1, a2) => appendRight(a1, a2))
  }

  // ex. 16
  def add1(l: List[Int]): List[Int] = {
    foldRight(l, Nil: List[Int])((a, b) => Cons(a + 1, b))
  }

  // ex. 17
  def toStringList(l: List[Double]): List[String] = {
    l match {
      case Nil => Nil
      case Cons(x, xs) => Cons(x.toString, toStringList(xs))
    }
//    foldRight(l, Nil: List[String])((a, b) => Cons(a.toString, b)) // Not wrong
//    foldLeft(l, Nil: List[String])((b, a) => Cons(a.toString, b)) // Wrong (return reversed list)
  }

  // ex. 18
  def map[A,B](l: List[A])(f: A => B): List[B] = {
    l match {
      case Nil => Nil
      case Cons(x, xs) => Cons(f(x), map(xs)(f))
    }
  }

  // ex. 19
  def filter[A](l: List[A])(f: A => Boolean): List[A] = {
    l match {
      case Nil => Nil
      case Cons(x, xs) if f(x) => Cons(x, filter(xs)(f))
      case Cons(x, xs) => filter(xs)(f)
    }
  }

  // ex. 20
  def flatMap[A, B](l: List[A])(f: A => List[B]): List[B] = {
    l match {
      case Nil => Nil: List[B]
      case Cons(x, xs) => append(f(x), flatMap(xs)(f))
    }
  }

  // ex. 21
  def filterWithFlatMap[A](l: List[A])(f: A => Boolean): List[A] = {
    flatMap(l)(a => if (f(a)) List(a) else Nil)
  }

  // ex. 22
  def addLists(a1: List[Int], a2: List[Int]): List[Int] ={
    (a1, a2) match {
      case (Nil, Nil) => Nil
      case (Cons(x1, xs1), Cons(x2, xs2)) => Cons((x1 + x2), addLists(xs1, xs2))
      case (Cons(_,_), Nil) => a1
      case (Nil, Cons(_,_)) => a2
    }
  }

  // ex. 23
  def mapList[A, B](a1: List[A], a2: List[A])(f: (A, A) => B): List[B] = {
    (a1, a2) match {
      case (Nil, Nil) => Nil
      case (Cons(x1, xs1), Cons(x2, xs2)) => Cons(f(x1, x2), mapList(xs1, xs2)(f))
      case _ => Nil
    }
  }

  // ex. 24
  def hasSubsequence[A](l: List[A], sub: List[A]): Boolean = {
    (l, sub) match {
      case (_, Nil) => true
      case (Cons(x1, xs1), Cons(x2, xs2)) => if (x1 == x2) hasSubsequence(xs1, xs2) else hasSubsequence(xs1, sub)
      case _ => false
    }
  }
}
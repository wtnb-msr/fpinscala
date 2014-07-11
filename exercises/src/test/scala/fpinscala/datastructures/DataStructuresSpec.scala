package fpinscala.datastructures

import fpinscala.datastructures.List._
import org.specs2.mutable.Specification

class DataStructuresSpec extends Specification {

  "ex. 1" should {
    "return" in {
      val x = List(1, 2, 3, 4, 5) match {
        case Cons(x, Cons(2, Cons(4, _))) => x
        case Nil => 42
        case Cons(x, Cons(y, Cons(3, Cons(4, _)))) => x + y
        case Cons(h, t) => h + List.sum(t)
        case _ => 101
      }
      x must_== 3
    }
  }

  "ex. 2" should {
    "return 1" in {
      tail(List(1, 2, 3, 4, 5)) must_== List(2, 3, 4, 5)
    }
    "return 2" in {
      tail(Nil) must_== Nil
    }
  }

  "ex. 3" should {
    "return 1" in {
      setHead(List(1, 2, 3, 4, 5), 10) must_== List(10, 2, 3, 4, 5)
    }
    "return 2" in {
      setHead(Nil, 10) must_== Cons(10, Nil)
    }
  }

  "ex. 4" should {
    "return 1" in {
      drop(List(1, 2, 3, 4, 5), 2) must_== List(3, 4, 5)
    }
    "return 2" in {
      drop(List(1, 2, 3, 4, 5), 6) must_== Nil
    }
  }

  "ex. 5" should {
    val f: Int => Boolean = (_ < 10)
    "return 1" in {
      dropWhile(List(1, 5, 9, 13, 17), f) must_== List(13, 17)
    }
    "return 2" in {
      dropWhile(List(1, 2, 3, 4, 5), f) must_== Nil
    }
  }

  "ex. 6" should {
    "return 1" in {
      init(List(1, 5, 9, 13, 17)) must_== List(1, 5, 9, 13)
    }
    "return 2" in {
      init(List(1)) must_== Nil
    }
  }

}

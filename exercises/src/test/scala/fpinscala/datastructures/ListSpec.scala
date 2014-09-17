package fpinscala.datastructures

import fpinscala.datastructures.List._
import org.specs2.mutable.Specification

class ListSpec extends Specification {

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

  "ex. 7" should {
    "return result1" in {
      val inputList = List(1.0, 2.0, 1.0, 2.0, 1.0, 1.0, 1.0, 1.0, 1.0)
      product2(inputList) must_== 4.0
    }
  }

  "ex. 8" should {
    "return result" in {
      val inputList = List(1, 2, 3)
      val result = foldRight(inputList, Nil: List[Int])(Cons(_, _))
      println("result" + result) // 何も変化がない
      result must_== inputList
    }
  }

  "ex. 9" should {
    "return result" in {
      val inputList = List(1, 2, 3, 4, 5, 6, 7, 8, 9)
      val result = List.length(inputList)
      result must_== 9
    }
    "return result1" in {
      val inputList = Nil
      val result = List.length(inputList)
      result must_== 0
    }
  }

  "ex. 10" should {
    "return result" in {
      val inputList = List(1, 2, 3, 4, 5, 6, 7, 8)
      val result = foldLeft(inputList, 0)(_ + _)
      result must_== 36
    }
  }

  "ex. 11" should {
    "return result sumLeft" in {
      val inputList = List(1, 2, 3, 4)
      val result = sumLeft(inputList)
      result must_== 10
    }
    "return result productLeft" in {
      val inputList = List(1.0, 2.0, 3.0, 4.0)
      val result = productLeft(inputList)
      result must_== 24.0
    }
    "return result lengthLeft" in {
      val inputList = List(1, 2, 3, 4)
      val result = lengthLeft(inputList)
      result must_== 4
    }
  }

  "ex. 12" should {
    "return reverse list" in {
      val inputList = List(1, 2, 3, 4)
      val result = reverse(inputList)
      result must_== List(4, 3, 2, 1)
    }
  }

//  "ex. 13" should { }

  "ex. 14" should {
    "return appended list" in {
      val inputList = List(1, 2, 3)
      val appendList = List(10, 11, 12)
      val result = appendRight(inputList, appendList)
      result must_== List(1, 2, 3, 10, 11, 12)
    }
    "return appended list" in {
      val inputList = Nil
      val appendList = List(10, 11, 12)
      val result = appendRight(inputList, appendList)
      result must_== List(10, 11, 12)
    }
    "return appended list" in {
      val inputList = List(1, 2, 3)
      val appendList = Nil
      val result = appendRight(inputList, appendList)
      result must_== List(1, 2, 3)
    }
  }

  "ex. 15" should {
    "return concatenated list" in {
      val inputList1 = List(1, 2, 3)
      val inputList2 = List(11, 12, 13)
      val inputList3 = List(21, 22, 23)
      val inputList = List(inputList1, inputList2, inputList3)
      val result = concatenate(inputList)
      result must_== List(1, 2, 3, 11, 12, 13, 21, 22, 23)
    }
  }

  "ex. 16" should {
    "return list by adding 1" in {
      val inputList = List(1, 2, 3, 4)
      val result = add1(inputList)
      result must_== List(2, 3, 4, 5)
    }
    "return Nil" in {
      val inputList = Nil
      val result = add1(inputList)
      result must_== Nil
    }
  }

  "ex. 17" should {
    "return string list" in {
      val inputList = List(1.0, 2.0, 3.0, 4.0)
      val result = toStringList(inputList)
      result must_== List("1.0", "2.0", "3.0", "4.0")
    }
  }

  "ex. 18" should {
    "return result" in {
      val inputList = List(1.0, 2.0, 3.0, 4.0)
      val result = List.map(inputList)(_.toString)
      result must_== List("1.0", "2.0", "3.0", "4.0")
    }
  }

  "ex. 19" should {
    "return result" in {
      val inputList = List(1, 10, 2, 9, 3, 8, 4, 7, 5, 6)
      val result = List.filter(inputList)(_ > 5)
      result must_== List(10, 9, 8, 7, 6)
    }
  }

  "ex. 20" should {
    "return result" in {
      val inputList = List(1, 2, 3, 4, 5)
      val result = List.flatMap(inputList)(i => List(i, i*i))
      result must_== List(1, 1, 2, 4, 3, 9, 4, 16, 5, 25)
    }
  }

  "ex. 21" should {
    "return result" in {
      val inputList = List(1, 10, 2, 9, 3, 8, 4, 7, 5, 6)
      val result = List.filterWithFlatMap(inputList)(_ > 5)
      result must_== List(10, 9, 8, 7, 6)
    }
  }

  "ex. 22" should {
    "return result" in {
      val result = addLists(List(1, 2, 3), List(10, 20, 30))
      result must_== List(11, 22, 33)
    }
    "return result" in {
      val result = addLists(List(1, 2, 3), List(10, 20, 30, 40, 50))
      result must_== List(11, 22, 33, 40, 50)
    }
  }

  "ex. 23" should {
    "return result" in {
      val result = mapList(List(1, 2, 3), List(10, 20, 30))((a1, a2) => (a1 * a2).toString)
      result must_== List("10", "40", "90")
    }
    "return result" in {
      val result = mapList(List(1, 2, 3), List(10, 20, 30, 40))((a1, a2) => (a1 * a2).toString)
      result must_== List("10", "40", "90")
    }
  }

  "ex. 24" should {
    "return true list has sub" in {
      val list = List(1, 2, 3, 4)
      hasSubsequence(list, List(1, 2)) must_== true
      hasSubsequence(list, List(2, 3)) must_== true
      hasSubsequence(list, List(3, 4)) must_== true
      hasSubsequence(list, List(1, 2, 3, 4)) must_== true
      hasSubsequence(list, Nil) must_== true
    }
    "return false list has no sub" in {
      val list = List(1, 2, 3, 4)
      hasSubsequence(list, List(1, 1)) must_== false
      hasSubsequence(list, List(2, 2)) must_== false
      hasSubsequence(list, List(3, 3)) must_== false
      hasSubsequence(list, List(4, 4)) must_== false
      hasSubsequence(list, List(1, 2, 3, 4, 5)) must_== false
    }
  }
}

package fpinscala.gettingstarted

import org.specs2.mutable.Specification
import fpinscala.gettingstarted.MyModule.fib
import fpinscala.gettingstarted.PolymorphicFunctions.isSorted

class GettingStartedSpec extends Specification {

  "fib" should {

    "OK" in {
      fib(0) must_== 0
      fib(1) must_== 1
      fib(7) must_== 13
    }

  }

  "isSorted" should {

    val gtInt = (a: Int, b: Int) => a >= b
    val gtString = (a: String, b: String) => a >= b

    "return true with sorted Int array" in {
      isSorted(Array(11, 5, 3, 1), gtInt) must beTrue
    }

    "return false with unsorted Int array" in {
      isSorted(Array(1, 3, 5, 11), gtInt) must beFalse
    }

    "return true with sorted String array" in {
      isSorted(Array("d", "c", "b", "a"), gtString) must beTrue
    }

    "return false with unsorted String array" in {
      isSorted(Array("a", "b", "c", "d"), gtString) must beFalse
    }

  }
}

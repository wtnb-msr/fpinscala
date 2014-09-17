package fpinscala.datastructures

import org.specs2.mutable.Specification

class TreeSpec extends Specification {

  val tree =
    Branch(
      Branch(
        Branch(
          Leaf(4),
          Leaf(2)),
        Branch(
          Leaf(1),
          Leaf(6))),
      Branch(
        Leaf(3),
        Leaf(5)))

  "ex. 3.25" should {
    "return" in {
      Tree.size(tree) must_== 11
    }
  }
  "ex. 3.26" should {
    "return" in {
      Tree.maximum(tree) must_== 6
    }
  }
  "ex. 3.27" should {
    "return" in {
      Tree.depth(tree) must_== 4
    }
  }
  "ex. 3.28" should {
    "return" in {
      Tree.depth(tree) must_== 4 // TODO
    }
  }
}

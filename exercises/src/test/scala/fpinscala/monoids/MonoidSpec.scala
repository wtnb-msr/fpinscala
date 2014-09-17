package fpinscala.monoids

import org.specs2.mutable.Specification

class MonoidSpec extends Specification {

  "ListFoldable" should {
    import ListFoldable._
    "foldRight" in {
      foldRight(List("a", "b", "c"))("")(_ + _) must_== "abc"
    }
    "foldLeft" in {
      foldLeft(List("a", "b", "c"))("")(_ + _) must_== "abc"
    }
    "foldMap" in {
      foldMap(List(1, 2, 3))(_.toString)(Monoid.stringMonoid) must_== "123"
    }
  }

  "IndexedSeqFoldable" should {
    import IndexedSeqFoldable._
    "foldRight" in {
      foldRight(IndexedSeq("a", "b", "c"))("")(_ + _) must_== "abc"
    }
    "foldLeft" in {
      foldLeft(IndexedSeq("a", "b", "c"))("")(_ + _) must_== "abc"
    }
    "foldMap" in {
      foldMap(IndexedSeq(1, 2, 3))(_.toString)(Monoid.stringMonoid) must_== "123"
    }
  }

  "StreamFoldable" should {
    import StreamFoldable._
    "foldRight" in {
      foldRight(Stream("a", "b", "c"))("")(_ + _) must_== "abc"
    }
    "foldLeft" in {
      foldLeft(Stream("a", "b", "c"))("")(_ + _) must_== "abc"
    }
  }

  "TreeFoldable" should {
    import TreeFoldable._
    "foldRight" in {
      foldRight(Branch(Branch(Leaf("a"), Leaf("b")), Branch(Leaf("c"), Leaf("d"))))("")(_ + _) must_== "abcd"
    }
    "foldLeft" in {
      foldLeft(Branch(Branch(Leaf("a"), Leaf("b")), Branch(Leaf("c"), Leaf("d"))))("")(_ + _) must_== "abcd"
    }
    "foldMap" in {
      foldMap(Branch(Branch(Leaf("1"), Leaf("2")), Branch(Leaf("3"), Leaf("4"))))(_.toInt)(Monoid.intAddition) must_== 10
    }
    "toList" in {
      toList(Branch(Branch(Leaf("a"), Leaf("b")), Branch(Leaf("c"), Leaf("d")))) must_== List("a", "b", "c", "d")
    }
  }

  "OptionFoldable" should {
    val addMonoid = new Monoid[Int] {
      def op(a1: Int, a2: Int) = a1 + a2
      val zero = 0
    }
    "test" in {
      OptionFoldable.foldMap(Some(1))(_ + 1)(addMonoid) must_== 2
    }
    "test1" in {
      OptionFoldable.foldMap(None: Option[Int])(_ + 1)(addMonoid) must_== 0
    }
    "toList1" in {
      OptionFoldable.toList(Some(1)) must_== List(1)
    }
    "toList2" in {
      OptionFoldable.toList(None) must_== Nil
    }
  }


}

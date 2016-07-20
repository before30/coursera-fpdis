package quickcheck

import common._

import org.scalacheck._
import Arbitrary._
import Gen._
import Prop._

abstract class QuickCheckHeap extends Properties("Heap") with IntHeap {

  property("min1") = forAll { a: Int =>
    val h = insert(a, empty)
    findMin(h) == a
  }

  property("gen1") = forAll { h: H =>
    val m = if (isEmpty(h)) 0 else findMin(h)
    findMin(insert(m, h)) == m
  }

  property("insTwoElemInEmpty") = forAll { (a1: Int, a2: Int) =>
    val h = insert(a2, insert(a1, empty))
    val smallest = if (a1 < a2) a1 else a2
    findMin(h) == smallest
  }

  property("insDelMin") = forAll { a: Int =>
    val h = insert(a, empty)
    val h1 = deleteMin(h)
    h1 == empty
  }

  property("meldMin") = forAll { (h1: H, h2: H) =>
    val min1 = findMin(h1)
    val min2 = findMin(h2)
    val m = meld(h1, h2)
    val minMeld = findMin(m)
    minMeld == min1 || minMeld == min2
  }

  property("remMin") = forAll { h: H =>
    def remMin(ts: H, as: List[Int]): List[Int] = {
      if (isEmpty(ts)) as
      else findMin(ts) :: remMin(deleteMin(ts), as)
    }
    val xs = remMin(h, Nil)
    xs == xs.sorted
  }

  property("meldMinMove") = forAll { (h1: H, h2: H) =>
    def remMin(ts: H, as: List[Int]): List[Int] = {
      if (isEmpty(ts)) as
      else findMin(ts) :: remMin(deleteMin(ts), as)
    }
    val meld1 = meld(h1, h2)
    val min1 = findMin(h1)
    val meld2 = meld(deleteMin(h1), insert(min1, h2))
    val xs1 = remMin(meld1, Nil)
    val xs2 = remMin(meld2, Nil)
    xs1 == xs2
  }

  property("nw1") = forAll { (h1: H, h2: H) =>
    val m1 = findMin(h1)
    val m2 = findMin(h2)
    val min = m1.min(m2)
    findMin(meld(deleteMin(h1),insert(min,h2)))==min
  }


  /**
    * If you insert any two elements into an empty heap,
    * finding the minimum of the resulting heap should
    * get the smallest of the two elements back.
    */
  property("findMin from a 2-element heap should yield the min") = forAll { (a: Int, b: Int) =>
    val h1 = insert(a, empty)
    val h2 = insert(b, h1)
    findMin(h2) == Math.min(a, b)
  }

  property("findMin from a 1-element heap should yield the element") = forAll { (a: Int) =>
    val h = insert(a, empty)
    findMin(h) == a
  }

  /**
    * If you insert an element into an empty heap, then
    * delete the minimum, the resulting heap should be empty.
    */
  property("insert and delete from an empty heap should yield an empty heap") = forAll { a: Int =>
    val h1 = insert(a, empty)
    val h2 = deleteMin(h1)
    h2 == empty
  }

  /**
    * Given any heap, you should get a sorted sequence
    * of elements when continually finding and deleting
    * minima. (Hint: recursion and helper functions are
    * your friends.)
    */
  property("findMin and deleteMin from a random heap should yield a sorted sequence") = forAll { h: H =>
    def rec(h: H): List[Int] = h match {
      case h if h == empty => Nil
      case h => findMin(h) :: rec(deleteMin(h))
    }
    val l = rec(h)
    (l, l.tail).zipped.forall(_ <= _)
  }

  /**
    * Finding a minimum of the melding of any two heaps
    * should return a minimum of one or the other.
    */
  property("findMin a meld heap should yield the min of the mins") = forAll { (h1: H, h2: H) =>
    findMin(meld(h1, h2)) == Math.min(findMin(h1), findMin(h2))
  }

  property("min after insert element gt min of a random heap should yield min") = forAll { (a: Int) =>
    val b = if (a == Int.MaxValue) a+1 else a
    val h1 = insert(b+1, insert(b, insert(b+2, empty)))
    val h2 = deleteMin(h1)
    h2 == insert(b+2, insert(b+1, empty))
  }

  property("2 insertions and 2 deletions should be empty") = forAll { (a: Int) =>
    val h1 = insert(a, insert(a, empty))
    val h2 = deleteMin(h1)
    !isEmpty(h2)
  }


  lazy val genHeap: Gen[H] = for {
    x <- arbitrary[Int]
    h <- oneOf(genHeap, const(empty))
  } yield insert(x, h)

  implicit lazy val arbHeap: Arbitrary[H] = Arbitrary(genHeap)

  def toList(h:H):List[Int] = if (isEmpty(h)) Nil else findMin(h) :: toList(deleteMin(h))
}

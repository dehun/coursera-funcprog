package quickcheck

import common._

import org.scalacheck._
import Arbitrary._
import Gen._
import Prop._
import Math.min

abstract class QuickCheckHeap extends Properties("Heap") with IntHeap {

  lazy val genHeap: Gen[H] = for {
    x <- arbitrary[Int]
    h <- oneOf(const(empty), genHeap)
  } yield insert(x, h)

  lazy val gen2ElHeap: Gen[H] = for {
    x <- arbitrary[Int]
    y <- arbitrary[Int]
    h <- const(empty)
  } yield insert(y, (insert(x, h)))

  lazy val gen3ElHeap: Gen[H] = for {
    x <- arbitrary[Int]
    y <- arbitrary[Int]
    z <- arbitrary[Int]
    h <- const(empty)
  } yield insert(z, insert(y, (insert(x, h))))

  implicit lazy val arbHeap: Arbitrary[H] = Arbitrary(genHeap)

  property("minimal heap") = forAll { (h: H) =>
    val m = if (isEmpty(h)) 0 else findMin(h)
    findMin(insert(m, h)) == m
  }

  property("empty heap isEmpty") = forAll (empty) { (h: H) =>
    isEmpty(h)
  }

  property("empty heap throws nosuch element exception on find") = forAll (empty) { (h: H) =>
    Prop.throws(classOf[NoSuchElementException]) {findMin(h)}
  }

  property("empty heap throws nosuch element exception on delete") = forAll (empty) { (h: H) =>
    Prop.throws(classOf[NoSuchElementException]) {deleteMin(h)}
  }

  property("2 element heap exhaustion") = forAll (gen2ElHeap) { (h: H) =>
    deleteMin(deleteMin(h)) == empty
  }

  property("3 element heap order preservation") = forAll (gen3ElHeap) { (h: H) =>
    val f = findMin(h)
    val s = findMin(deleteMin(h))
    val t = findMin(deleteMin(deleteMin(h)))
    (f <= s) && (s <= t)
  }

  property("2 element heap with 3 element heap merging:: minimal is minimal of both") = forAll(gen2ElHeap, gen3ElHeap) { (lhs:H, rhs:H) =>
    findMin(meld(lhs, rhs)) == min(findMin(lhs), findMin(rhs))
  }

  property("2 element heap with 3 element heap merging:: exhaustion") = forAll(gen2ElHeap, gen3ElHeap) { (lhs: H, rhs: H) =>
    List.range(0, 5).foldLeft(meld(lhs, rhs))((acc: H, _: Int) => {
      deleteMin(acc)
    }) == empty
  }

  property("melding empty heaps is empty heap") = forAll(empty, empty) {
    (lhs:H, rhs:H) => {
      meld(lhs, rhs) == empty
    }
  }

  property("melding with empty heaps is identity") = forAll(gen3ElHeap) {
    (h:H) => {
      (meld(empty, h) == h) && (meld(h, empty) == h)
    }
  }

  property("reinsertion into 3elem heap") = forAll(gen3ElHeap) {
    (h:H) => {
      val m = findMin(h)
      findMin(insert(m, deleteMin(h))) == m
    }
  }

  property("heap-sort should work") = forAll(arbitrary[List[Int]]) {
    (xs:List[Int]) => {
      var h = xs.foldLeft(empty)((acc, x) => insert(x, acc))
      var ys = List[Int]()
      while (!isEmpty(h)) {
        ys = findMin(h) :: ys
        h = deleteMin(h)
      }
      xs.sorted.reverse == ys
    }

  }
}

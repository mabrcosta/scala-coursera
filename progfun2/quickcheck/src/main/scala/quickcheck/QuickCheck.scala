package quickcheck

import common._

import org.scalacheck._
import Arbitrary._
import Gen._
import Prop._

abstract class QuickCheckHeap extends Properties("Heap") with IntHeap {

  lazy val genHeap: Gen[H] = for (
    v <- arbitrary[Int];
    m <- oneOf(const(empty), genHeap)) yield insert(v, m)

  implicit lazy val arbHeap: Arbitrary[H] = Arbitrary(genHeap)

  //Bogus 1, 2 and 5
  property("meld1") = forAll { (h1: H, h2: H) =>
    val h = meld(h1, h2)
    findMin(h) == findMin(h1) || findMin(h) == findMin(h2)
  }

  //Bogus 4
  property("delMin3") = forAll { (a: Int, b: Int, c: Int) =>
    val h = insert(c, insert(b, insert(a, empty)))
    findMin(deleteMin(deleteMin(h))) == (c max (a max b))
  }

  //Bogus 2 and 3
  property("deletemin1") = forAll { (a1: Int, a2: Int) =>
    val h = insert(a2, insert(a1, empty))
    val hmax = deleteMin(h)
    val maxVal = a1 max a2
    findMin(hmax) == maxVal
  }

}

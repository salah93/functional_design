package quickcheck

import java.util.NoSuchElementException

import common._
import org.scalacheck._
import Arbitrary._
import Gen._
import Prop._

abstract class QuickCheckHeap extends Properties("Heap") with IntHeap {

  val genEmpty: Gen[H] = const(empty)
  lazy val genHeap: Gen[H] = oneOf(
    genEmpty,
    for {
      v <- arbitrary[Int]
      h <- genHeap
    } yield insert(v, h)
  )

  implicit lazy val arbHeap: Arbitrary[H] = Arbitrary(genHeap)

  property("gen1") = forAll { h: H =>
    val m = if (isEmpty(h)) 0 else findMin(h)
    findMin(insert(m, h)) == m
  }

  property("min1") = forAll { a: Int =>
    val h = insert(a, empty)
    findMin(h) == a
  }

  property("empty") = forAll { a: Int =>
    val h = insert(a, empty)
    val new_h = deleteMin(h)
    isEmpty(new_h)
  }

  property("min2") = forAll { (a: Int, b: Int) =>
    val h = insert(b, insert(a, empty))
    val min = if (ord.lteq(a, b)) a else b
    findMin(h) == min
  }

  property("get all nodes back") = forAll { (a: Int, b: Int) =>
    val h = insert(b, insert(a, empty))
    val (min, min2) = if (ord.lteq(a, b)) (a, b) else (b, a)
    try{
     val m = findMin(h)
     val new_heap = deleteMin(h)
     val m2 = findMin(new_heap)
     val e = deleteMin(new_heap)
      isEmpty(e) && m == min && m2 == min2
    }
    catch {
      case e: NoSuchElementException => false
    }
  }

  property("min of two melded heaps") = forAll { (h1: H, h2: H)  =>
    val h1min: Option[A] = if (isEmpty(h1)) None else Some(findMin(h1))
    val h2min: Option[A] = if (isEmpty(h2)) None else Some(findMin(h2))
    val combined = meld(h1, h2)
    if (isEmpty(combined))
      true
    else{
      val min = h1min match {
        case None => h2min.get
        case Some(a) => h2min match {
          case None => a
          case Some(b) =>
            if (ord.lteq(a, b)) a else b
        }
      }
      findMin(combined) == min
    }
  }

  property("is sorted") = forAll { h: H =>
    def loop(heap: H): List[A] = {
      if (isEmpty(heap)) Nil
      else findMin(heap) :: loop(deleteMin(heap))
    }

    val list = loop(h)
    (for {
      i <- 0 until list.length - 1
    } yield ord.lteq(list(i), list(i + 1))).forall(a=> a)
  }
  property("delete Min") = forAll { (a: Int, b: Int, c: Int) =>
    val l = List(a, b, c).sorted
    val min = l(0)
    val second_min = l(1)
    val third_min = l(2)
    var h = insert(third_min, insert(second_min, insert(min, empty)))
    var ret = findMin(h) == min
    h = deleteMin(h)
    ret = ret && findMin(h) == second_min
    h = deleteMin(h)
    ret && findMin(h) == third_min && isEmpty(deleteMin(h))
  }
}
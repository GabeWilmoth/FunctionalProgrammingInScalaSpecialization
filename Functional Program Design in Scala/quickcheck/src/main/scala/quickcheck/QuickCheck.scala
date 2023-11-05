package quickcheck

import org.scalacheck.*
import Arbitrary.*
import Gen.*
import Prop.forAll
import scala.annotation.tailrec                                          

abstract class QuickCheckHeap extends Properties("Heap") with IntHeap:
  lazy val genMap: Gen[Map[Int,Int]] = oneOf(                           
    const(Map.empty[Int,Int]),
    for
      k <- arbitrary[Int]
      v <- arbitrary[Int]
      m <- oneOf(const(Map.empty[Int,Int]), genMap)
    yield m.updated(k, v)
  )

  lazy val genHeap: Gen[H] =                                             
    for
      node <- arbitrary[A]                                            
      heap <- oneOf(const(empty), genHeap)
    yield insert(node, heap)

  given Arbitrary[H] = Arbitrary(genHeap)

  property("gen1") = forAll { (h: H) =>                                 
    val m = if isEmpty(h) then 0 else findMin(h)
    findMin(insert(m, h)) == m
  }

  property("min1") = forAll { (a: Int) =>
    val h: H = insert(a, empty)
    findMin(h) == a
  }

  property("gen2") = forAll { (a: A, b: A) =>
    val h1: H = insert(a, empty)
    val h2: H = insert(b, h1)
    val minab: A = if a < b then a else b
    findMin(h2) == minab
  }

  property("gen3") = forAll { (a: A) =>
    val h: H = insert(a, empty)
    deleteMin(h) == empty
  }

  property("gen4") = forAll { (h: H) =>
    @tailrec
    def helper(acc: List[A], h: H): Boolean =
      if isEmpty(h) then acc == acc.sorted.reverse
      else helper(findMin(h) :: acc, deleteMin(h))

    helper(List(), h)
  }

  property("gen5") = forAll { (h1: H, h2: H) =>
    if !isEmpty(h1) && !isEmpty(h2) then
      val minh1: A = findMin(h1)
      val minh2: A = findMin(h2)
      val min12: A = if minh1 < minh2 then minh1 else minh2
      findMin(meld(h1, h2)) == min12

    else if isEmpty(h1) then findMin(meld(h1, h2)) == findMin(h2)
    else if isEmpty(h2) then findMin(meld(h1, h2)) == findMin(h1)
    else true
  }

  property("gen6") = forAll { (h1: H, h2: H) =>
    @tailrec
    def helper(h1: H, h2: H): Boolean =
      if   isEmpty(h1) && isEmpty(h2)
      then true
      else findMin(h1) == findMin(h2) && helper(deleteMin(h1), deleteMin(h2))

    val (m1, m2) = (findMin(h1), findMin(h2))
    helper(meld(insert(m2, h1), deleteMin(h2)),
           meld(deleteMin(h1), insert(m1, h2)))
  }
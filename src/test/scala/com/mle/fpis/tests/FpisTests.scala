package com.mle.fpis.tests

import org.scalatest.FunSuite
import Chapter2And3._

/**
 *
 * @author mle
 */
class FpisTests extends FunSuite {
  test("can run test") {
    assert(1 === 2 - 1)
  }
  // 0 1 1 2 3 5 8 13 21
  test("fibonacci") {
    assert(fib(1) === 0)
    assert(fib(2) === 1)
    assert(fib(3) === 1)
    assert(fib(4) === 2)
    assert(fib(5) === 3)
    assert(fib(6) === 5)
    assert(fib(7) === 8)
    assert(fib(8) === 13)
    assert(fib(9) === 21)
  }
  test("isSorted") {
    val yes = Array(1, 2, 3, 4, 5)
    val no = Array(1, 4, 3)
    assert(isSorted[Int](yes, _ < _) === true)
    assert(isSorted[Int](no, _ < _) === false)
  }

  test("partial1") {
    val sum: (Int, Int) => Int = _ + _
    val init = 3
    val addThree = partial1(init, sum)
    assert(addThree(666) === 669)
  }

  import Chapter2And3.List

  test("list") {
    assert(List.xValue === 3)
  }
  test("tail") {
    val l = List(1, 2, 3)
    assert(List.tail(l) === List(2, 3))
  }
  test("drop") {
    val l = List(1, 2, 3)
    assert(List.drop(l, 2) === List(3))
  }
  test("dropWhile") {
    val l = List(1, 2, 3, 4, 5, 6)
    assert(List.dropWhile(l)(_ < 3) === List(3, 4, 5, 6))
  }
  test("setHead") {
    val l = List(1, 2, 3)
    val l2 = List.setHead(l, 666)
    assert(l2 === List(666, 2, 3))
  }
  test("init") {
    val l = List(1, 2, 3, 4)
    assert(List.init(l) === List(1, 2, 3))
  }
  test("foldRight") {
    val list = List.foldRight(List(1, 2, 3), Nil: List[Int])(Cons(_, _))
    assert(list === List(1, 2, 3))
  }
  test("length") {
    assert(List.length(List(1, 2, 3, 4)) === 4)
  }
  test("foldLeft") {
    val l = List(1, 2, 3, 4)
    assert(List.foldLeft(l, 0)(_ + _) === 10)
  }
  test("productLeft") {
    val l = List[Double](1, 2, 3, 4)
    assert(List.productLeft(l) === 24)
  }
  test("lengthLeft") {
    assert(List.lengthLeft(List(666, 2, 3, 4)) === 4)
  }
  test("reverse") {
    assert(List.reverse(List(1, 2, 3)) === List(3, 2, 1))
  }
  test("appendElem") {
    val l = List(1, 2, 3)
    assert(List.appendElem(l, 4) === List(1, 2, 3, 4))
  }
  test("append") {
    val a1 = List(1, 2, 5)
    val a2 = List(3, 4)
    assert(List.append(a1, a2) === List(1, 2, 5, 3, 4))
  }
  //  test("flatten") {
  //    val l = List(List(1), List(2), List(4, 3))
  //    assert(List.flatten(l) === List(1, 2, 4, 3))
  //  }
  test("flatten2") {
    val l = List(List(1), List(2), List(4, 3))
    assert(List.flatten2(l) === List(1, 2, 4, 3))
  }
  test("identity") {
    val l = List(1, 2, 3, 4)
    assert(List.identity(l) === l)
  }
  test("add one") {
    val l = List(2, 3, 1, 666)
    assert(List.addOne(l) === List(3, 4, 2, 667))
  }
  test("add one using map") {
    val l = List(2, 3, 1, 666)
    assert(List.addOneUsingMap(l) === List(3, 4, 2, 667))
  }
  test("filter") {
    val l = List(2, 5, 1, 2, 3, 666, 7, 10, 9)
    val onlyEven = List.filter(l)(_ % 2 == 0)
    assert(onlyEven === List(2, 2, 666, 10))
  }
  test("flatMap") {
    val l = List(1, 2, 3)
    val flatMapped = List.flatMap(l)(i => List(i, i))
    assert(flatMapped === List(1, 1, 2, 2, 3, 3))
  }
  test("filter using flatMap") {
    val l = List(2, 5, 1, 2, 3, 666, 7, 10, 9)
    val onlyEven = List.filter(l)(_ % 2 == 0)
    assert(onlyEven === List(2, 2, 666, 10))
  }
  test("add elements of two lists") {
    val l1 = List(2, 5, 32)
    val l2 = List(1, 2, 3)
    assert(List.addElements(l1, l2) === List(3, 7, 35))
  }
  test("add elements with generic method") {
    val l1 = List(2, 5, 32)
    val l2 = List(1, 2, 3)
    assert(List.computeLists(l1, l2)(_ + _) === List(3, 7, 35))
  }
  test("has subsequence") {
    val l = List(2, 3, 6, 666, 1, 2, 3)
    assert(List.hasSubsequence(l, List(2, 3)))
    assert(List.hasSubsequence(l, List(1, 2, 3)))
    assert(List.hasSubsequence(l, List(3, 6, 666)))
    assert(List.hasSubsequence(Nil, Nil))
    assert(List.hasSubsequence(l, Nil))
    assert(!List.hasSubsequence(l, List(1, 2, 3, 4)))
    assert(!List.hasSubsequence(l, List(5)))
    assert(!List.hasSubsequence(Nil, List(1, 2, 3)))
  }
  val t = Branch[Int](Branch[Int](Branch[Int](Leaf(1), Leaf(4)), Branch[Int](Leaf(2), Leaf(3))), Branch(Leaf(2), Leaf(5)))
  val tDouble = Branch[Int](Branch[Int](Branch[Int](Leaf(2), Leaf(8)), Branch[Int](Leaf(4), Leaf(6))), Branch(Leaf(4), Leaf(10)))

  /**
   * b
   * b         b
   * b     b     2 5
   * 1 4   2 3
   */


  test("Tree.size") {
    assert(Tree.size(t) === 11)
  }
  test("Tree.maximum") {
    assert(Tree.maximum(t) === 5)
  }
  test("Tree.depth") {
    assert(Tree.depth(Leaf(555)) === 0)
    assert(Tree.depth(t) === 3)
  }
  test("Tree.map") {
    assert(Tree.map(t)(_ * 2) === tDouble)
  }
  test("Tree.sizeUsingFold") {
    assert(Tree.sizeUsingFold(t) === 11)
  }
  test("Tree.maximumUsingFold") {
    assert(Tree.maximumUsingFold(t) === 5)
  }
  test("Tree.depthUsingFold") {
    assert(Tree.depthUsingFold(Leaf(555)) === 0)
    assert(Tree.depthUsingFold(t) === 3)
  }
  test("Tree.mapUsingFold") {
    assert(Tree.mapUsingFold(t)(_ * 2) === tDouble)
  }

  import Chapter4._

  test("sequence[A](List[Option[A]])") {
    val in1 = scala.List(Some(1), Some(2), None, Some(3))
    assert(Chapter4.sequence(in1) === None)
    val in2 = scala.List(Some("a"), Some("b"))
    assert(Chapter4.sequence(in2) === Some(scala.List("a", "b")))
  }
  test("sequenceInTermsOfTraverse") {
    val in1 = scala.List(Some(1), Some(2), None, Some(3))
    assert(Chapter4.sequenceInTermsOfTraverse(in1) === None)
    val in2 = scala.List(Some("a"), Some("b"))
    assert(Chapter4.sequenceInTermsOfTraverse(in2) === Some(scala.List("a", "b")))
  }

  import Chapter5._

  test("streams") {
    assert(Stream(1, 2, 3, 4, 5).take(2).toList === scala.List(1, 2))
    assert(Empty.take(5) === Empty)
  }

  test("Stream.takeWhile") {
    assert(Empty.takeWhile(_ => true) === Empty)
    assert(Stream(1, 2, 3, 4, 5).takeWhile(_ < 4).toList === Stream(1, 2, 3).toList)
  }
  test("Stream.exists") {
    assert(Stream(1, 2, 3, 4).exists(_ == 3))
    assert(!Stream(1, 2, 4).exists(_ == 3))
  }
  test("Stream.takeWhileUsingFoldRight") {
    assert(Empty.takeWhileUsingFoldRight(_ => true) === Empty)
    assert(Stream(1, 2, 3, 4, 5).takeWhileUsingFoldRight(_ < 4).toList === Stream(1, 2, 3).toList)
  }
  test("Stream.append") {
    val result = Stream(1, 2).append(Stream(3, 4))
    assert(result.toList === scala.List(1, 2, 3, 4))
  }
  test("Stream.flatMap") {
    val s = Stream(1, 2, 3)
    val result = s.flatMap(i => Stream(i * 2, i * 3))
    assert(result.toList === scala.List(2, 3, 4, 6, 6, 9))
  }
  test("ones") {
    assert(Stream.ones.map(_ + 1).exists(_ % 2 == 0))
  }
  test("Stream.fibs") {
    assert(Stream.fibs.take(7).toList === scala.List(0, 1, 1, 2, 3, 5, 8))
  }
  test("Stream.onesUsingUnfold") {
    assert(Stream.onesUsingUnfold.take(3).toList === scala.List(1, 1, 1))
  }
  test("Stream.fromUsingUnfold") {
    assert(Stream.fromUsingUnfold(4).take(3).toList === scala.List(4, 5, 6))
  }
  test("Stream.fibsUsingUnfold") {
    assert(Stream.fibsUsingUnfold.take(7).toList === scala.List(0, 1, 1, 2, 3, 5, 8))
  }
  test("Stream.constantUsingUnfold") {
    assert(Stream.constantUsingUnfold(6).take(5).toList === scala.List(6, 6, 6, 6, 6))
  }
  test("Stream.map") {
    assert(Stream.map(Stream(1, 2, 3))(_ * 2).toList === scala.List(2, 4, 6))
  }
  test("Stream.zipAll") {
    assert((Some(1), None) ===(Some(1), None))
    val s1 = Stream(1, 2)
    val s2 = Stream(3, 4, 5, 6)
    val actual = Stream.zipAll(s1, s2)
    val expected = Stream((Some(1), Some(3)), (Some(2), Some(4)), (None, Some(5)), (None, Some(6))).toList
    assert(actual.toList(1)._2.get === 4)
    assert(actual.toList(1)._2.get === expected.toList(1)._2.get)
    assert(actual.toList(2)._1.isEmpty)
  }
  test("Stream.startsWith") {
    assert(Stream.startsWith(Stream(1, 2, 3), Stream(1, 2)))
    assert(Stream.startsWith(Stream.from(1), Stream(1, 2, 3, 4, 5)))
    assert(!Stream.startsWith(Stream.from(1), Stream(1, 2, 3, 5)))
  }
  test("Stream.tails") {
    val actual = Stream(1, 2, 3).tails // Stream.tails(Stream(1, 2, 3))
    val expected = Stream(Stream(1, 2, 3), Stream(2, 3), Stream(3), Stream())
    assert(actual.map(_.toList).toList === expected.map(_.toList).toList)
  }
  test("Stream.scanRight") {
    assert(Stream(1, 2, 3).scanRight(0)(_ + _).toList === scala.List(6, 5, 3, 0))
  }

  import Chapter6._

  test("erroneousRollDie has (off-by-one) error") {
    assert(erroneousRollDie(Simple(5))._1 === 0)
  }
  test("positiveLessThan") {
    //    val numbers = (42 to 42 + 100).map(Simple(_)).map(positiveLessThan(13)).map(_._1)
    //    println(numbers)
  }
  test("rollDie") {
    val dies = (42 to 42 + 10000).map(Simple(_)).map(rollDie).map(_._1)
    assert(dies.forall(die => die > 0 && die < 7))
    (1 to 6).foreach(i => assert(dies.exists(_ == i)))
  }
  test("State for comprehension") {
    val ns: Rand[scala.List[Int]] =
      int.flatMap(x => int.flatMap(y => ints(x).map(xs => xs.map(_ % y))))

    val ns2: Rand[scala.List[Int]] = for {
      x <- int
      y <- int
      xs <- ints(x)
    } yield xs.map(_ % y)

    def run(xs: Rand[scala.List[Int]]) = xs.run(Simple(5))._1

    assert(run(ns) === run(ns2))
  }
  test("State.sequence") {
    val (v1, s1) = int.run(Simple(42))
    val (v2, _) = int.run(s1)

    val (list, _) = State.sequence(scala.List(int, int)).run(Simple(42))
    assert(list === scala.List(v1, v2))
  }
  test("simulateMachine") {
    val initState: Machine = Machine(locked = true, candies = 5, coins = 10)
    val moves = simulateMachine(scala.List(Coin, Turn, Coin, Turn, Coin, Turn, Coin, Turn))
    val ((coins, candies), _) = moves.run(initState)
    assert(coins === 14)
    assert(candies === 1)
  }
}

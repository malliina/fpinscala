package com.mle.fpis.tests

import org.scalatest.FunSuite
import Exercises._

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

  import Exercises.List

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


}

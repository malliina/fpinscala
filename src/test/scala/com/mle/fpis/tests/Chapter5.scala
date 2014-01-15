package com.mle.fpis.tests

/**
 *
 * @author mle
 */
object Chapter5 {

  sealed abstract class Stream[+A] {
    def uncons: Option[Cons[A]]

    def isEmpty: Boolean = uncons.isEmpty

    def toList: List[A] = uncons map (str => str.head :: str.tail.toList) getOrElse Nil

    def take(n: Int): Stream[A] =
      if (n == 0) Empty
      else uncons map (str => Stream.cons(str.head, str.tail.take(n - 1))) getOrElse Empty

//    def takeWhile(p: A => Boolean): Stream[A] = ???
  }

  object Empty extends Stream[Nothing] {
    val uncons = None
  }

  sealed abstract class Cons[+A] extends Stream[A] {
    def head: A

    def tail: Stream[A]

    val uncons = Some(this)
  }

  object Stream {
    def empty[A]: Stream[A] = Empty

    def cons[A](hd: => A, tl: => Stream[A]): Stream[A] = new Cons[A] {
      lazy val head = hd
      lazy val tail = tl
    }

    def apply[A](as: A*): Stream[A] =
      if (as.isEmpty) Empty else cons(as.head, apply(as.tail: _*))
  }

}

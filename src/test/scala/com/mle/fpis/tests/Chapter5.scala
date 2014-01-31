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

    def takeWhile(p: A => Boolean): Stream[A] =
      uncons map (str => if (p(str.head)) Stream.cons(str.head, str.tail.takeWhile(p)) else Empty) getOrElse Empty

    def foldRight[B](z: => B)(f: (A, => B) => B): B // =
    //      uncons match {
    //        case Some(c) => f(c.head, c.tail.foldRight(z)(f))
    //        case None => z
    //      }

    def exists(p: A => Boolean): Boolean =
      foldRight(false)((a, b) => p(a) || b)

    /**
     * Stream(1, 2, 3).forall(a => a < 4)
     * Stream(1, 2, 3).foldRight(true)((a, b) => a < 4 && b)
     * 1 < 4 && Stream(2, 3).foldRight(true)((a, b) => a < 4 && b)
     * 1 < 4 && (2 < 4 && Stream(3).foldRight(true)((a, b) => a < 4 && b))
     * 1 < 4 && (2 < 4 && (3 < 4 && Empty.foldRight(true)((a, b) => a < 4 && b)))
     * 1 < 4 && (2 < 4 && (3 < 4 && (true)))
     * true && true && true && true
     * true
     *
     * b is the foldRight on the tail
     */
    def forAll(p: A => Boolean): Boolean =
      foldRight(true)((a, b) => p(a) && b)

    def takeWhileUsingFoldRight(p: A => Boolean): Stream[A] =
      foldRight[Stream[A]](Empty)((a, b) => if (p(a)) Stream.cons(a, b) else Empty)

    def unconsUsingFoldRight: Option[Cons[A]] =
      foldRight[Option[Cons[A]]](None)((a, b) => Some(Stream.cons2(a, b.getOrElse(Empty))))

    def map[B](f: A => B): Stream[B] =
      foldRight[Stream[B]](Empty)((a, b) => Stream.cons(f(a), b))

    def filter(p: A => Boolean): Stream[A] =
      foldRight[Stream[A]](Empty)((a, b) => if (p(a)) Stream.cons(a, b) else b)

    def appendSingle[B >: A](elem: B): Stream[B] =
      foldRight[Stream[B]](Stream.cons(elem, Empty))((a, b) => Stream.cons(a, b))

    def append[B >: A](s: Stream[B]): Stream[B] =
      foldRight[Stream[B]](s)((a, b) => Stream.cons(a, b))

    def flatMap[B](f: A => Stream[B]): Stream[B] =
      foldRight[Stream[B]](Empty)((a, b) => f(a) append b)
  }

  object Empty extends Stream[Nothing] {
    val uncons = None

    def foldRight[B](z: => B)(f: (Nothing, => B) => B): B = z
  }

  sealed abstract class Cons[+A] extends Stream[A] {
    def head: A

    def tail: Stream[A]

    val uncons = Some(this)

    def foldRight[B](z: => B)(f: (A, => B) => B): B =
      f(head, tail.foldRight(z)(f))
  }

  object Stream {
    def empty[A]: Stream[A] = Empty

    def cons[A](hd: => A, tl: => Stream[A]): Stream[A] = cons2(hd, tl)

    def cons2[A](hd: => A, tl: => Stream[A]): Cons[A] = new Cons[A] {
      lazy val head = hd
      lazy val tail = tl
    }

    def apply[A](as: A*): Stream[A] =
      if (as.isEmpty) Empty else cons(as.head, apply(as.tail: _*))

    val ones: Stream[Int] = cons(1, ones)

    def constant[A](a: A): Stream[A] = cons(a, constant(a))

    def from(n: Int): Stream[Int] = cons(n, from(n + 1))

    def fibs: Stream[Int] = {
      def fibsInner(prevPrev: Int, prev: Int): Stream[Int] =
        cons(prevPrev, fibsInner(prev, prev + prevPrev))
      fibsInner(0, 1)
    }

    def unfold[A, S](z: S)(f: S => Option[(A, S)]): Stream[A] =
      f(z).map(p => cons(p._1, unfold(p._2)(f))).getOrElse(Empty)

    val onesUsingUnfold: Stream[Int] = unfold(1)(i => Some(1, 1))

    def fromUsingUnfold(n: Int): Stream[Int] =
      unfold(n)(i => Some((i, i + 1)))
  }

}

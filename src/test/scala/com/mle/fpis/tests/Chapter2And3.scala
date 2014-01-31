package com.mle.fpis.tests

/**
 *
 * @author Michael
 */
object Chapter2And3 {
  // Exercise 2.1

  // 0 1 1 2 3 5 8 13 21
  def fib(n: Int): Int = {
    @annotation.tailrec
    def fibAcc(n: Int, latest: Int, prev: Int): Int = {
      if (n == 0) latest
      else fibAcc(n - 1, latest + prev, latest)
    }
    if (n == 1) 0
    else if (n == 2) 1
    else fibAcc(n - 2, 1, 0)
  }

  def fibNonTailRec(n: Int): Int = {
    if (n == 1) 0
    else if (n == 2) 1
    else fib(n - 1) + fib(n - 2)
  }

  // Exercise 2.2
  def isSorted[A](as: Array[A], gt: (A, A) => Boolean): Boolean = {
    if (as.size <= 1) true
    else if (gt(as(0), as(1))) isSorted(as.tail, gt)
    else false
  }

  def partial1[A, B, C](a: A, f: (A, B) => C): B => C =
    b => f(a, b)

  // E. 2.3,4,5
  def curry[A, B, C](f: (A, B) => C): A => (B => C) =
    a => b => f(a, b)

  def uncurry[A, B, C](f: A => B => C): (A, B) => C =
    (a, b) => f(a)(b)

  def compose[A, B, C](f: B => C, g: A => B): A => C =
    a => f(g(a))

  // E3.1
  sealed trait List[+A]

  case object Nil extends List[Nothing]

  case class Cons[+A](head: A, tail: List[A]) extends List[A]

  object List {
    def sum(ints: List[Int]): Int = ints match {
      case Nil => 0
      case Cons(x, xs) => x + sum(xs)
    }

    def product(ds: List[Double]): Double = ds match {
      case Nil => 1.0
      case Cons(0.0, _) => 0.0
      case Cons(x, xs) => x * product(xs)
    }

    def apply[A](as: A*): List[A] =
      if (as.isEmpty) Nil
      else Cons(as.head, apply(as.tail: _*))

    val example = Cons(1, Cons(2, Cons(3, Nil)))
    val example2 = List(1, 2, 3)
    val total = sum(example)

    val xValue = List(1, 2, 3, 4, 5) match {
      case Cons(x, Cons(2, Cons(4, _))) => x
      case Nil => 42
      case Cons(x, Cons(y, Cons(3, Cons(4, _)))) => x + y
      case Cons(h, t) => h + sum(t)
    }

    def tail[A](l: List[A]): List[A] =
      l match {
        case Nil => throw new NoSuchElementException
        case Cons(x, xs) => xs
      }

    def drop[A](l: List[A], n: Int): List[A] =
      if (n == 0) l
      else drop(tail(l), n - 1)

    def dropWhile[A](l: List[A])(f: A => Boolean): List[A] =
      l match {
        case Nil => Nil
        case Cons(x, xs) =>
          if (f(x)) dropWhile(xs)(f)
          else l
      }

    def setHead[A](l: List[A], head: A): List[A] =
      Cons(head, tail(l))

    def init[A](l: List[A]): List[A] =
      l match {
        case Nil => throw new NoSuchElementException
        case Cons(x, Nil) => Nil
        case Cons(x, Cons(y, Nil)) => Cons(x, Nil)
        case Cons(x, Cons(y, xs)) => Cons(x, Cons(y, init(xs)))
      }

    /**
     * foldRight(List(1, 2, 3), 0)((a, b) => a + b)
     * 1 + foldRight(List(2, 3), 0)((a, b) => a + b)
     * 1 + (2 + foldRight(List(3), 0)((a, b) => a + b))
     * 1 + (2 + (3 + foldRight(Nil, 0)((a, b) => a + b)))
     * 1 + (2 + (3 + (0)))
     * 6
     */

    def foldRight[A, B](l: List[A], z: B)(f: (A, B) => B): B =
      l match {
        case Nil => z
        case Cons(x, xs) => f(x, foldRight(xs, z)(f))
      }

    def length[A](l: List[A]): Int =
      foldRight(l, 0)((a, acc) => acc + 1)

    /**
     * foldLeft(List(1, 2, 3), 0)((acc, a) => acc + a)
     * foldLeft(List(2, 3), 1)((acc, a) => acc + a)
     * foldLeft(List(3), 3)((acc, a) => acc + a)
     * foldLeft(Nil, 6)((acc, a) => acc + a)
     * 6
     */
    def foldLeft[A, B](l: List[A], z: B)(f: (B, A) => B): B =
      l match {
        case Nil => z
        case Cons(x, xs) => foldLeft(xs, f(z, x))(f)
      }

    def sumLeft(l: List[Int]): Int =
      foldLeft(l, 0)(_ + _)

    def productLeft(l: List[Double]): Double =
      foldLeft(l, 1.0)(_ * _)

    def lengthLeft[A](l: List[A]): Int =
      foldLeft(l, 0)((acc, a) => acc + 1)

    /**
     * identity(List(1, 2, 3))
     * foldRight(List(1, 2, 3), Nil)((a, acc) => Cons(a, acc))
     * Cons(1, foldRight(List(2, 3), Nil)((a, acc) => Cons(a, acc)))
     * Cons(1, Cons(2, foldRight(List(3), Nil)((a, acc) => Cons(a, acc)))
     * Cons(1, Cons(2, Cons(3, foldRight(Nil, Nil)((a, acc) => Cons(a, acc)))
     * Cons(1, Cons(2, Cons(3, Nil)))
     * List(1, 2, 3)
     */
    def identity[A](l: List[A]): List[A] =
      foldRight(l, Nil: List[A])((a, acc) => Cons(a, acc))

    /**
     * reverse(List(1,2,3))
     * foldLeft(List(1,2,3), Nil)((acc, a) => Cons(a, acc))
     * foldLeft(List(2,3), Cons(1, Nil))((acc, a) => Cons(a, acc))
     * foldLeft(List(3), Cons(2, Cons(1, Nil)))((acc, a) => Cons(a, acc))
     * foldLeft(Nil, Cons(3, Cons(2, Cons(1, Nil)))((acc, a) => Cons(a, acc))
     * Cons(3, Cons(2, Cons(1, Nil))
     * List(3, 2, 1)
     */
    def reverse[A](l: List[A]): List[A] =
      foldLeft(l, Nil: List[A])((acc, a) => Cons(a, acc))

    def foldLeftInTermsOfFoldRight[A, B](l: List[A], z: B)(f: (B, A) => B): B =
      ???

    def foldRightInTermsOfFoldLeft[A, B](l: List[A], z: B)(f: (A, B) => B): B =
      ???

    def appendElem[A](l: List[A], elem: A): List[A] =
      foldRight(l, Cons(elem, Nil))((a, acc) => Cons(a, acc))

    // use either foldleft or foldright
    def append[A](a1: List[A], a2: List[A]): List[A] =
      foldRight(a1, a2)((a, acc) => Cons(a, acc))

    def flatten[A](l: List[List[A]]): List[A] =
      l match {
        case Nil => Nil
        case Cons(x, xs) => foldRight(xs, x)(append)
      }

    def flatten2[A](l: List[List[A]]): List[A] = {
      def go(acc: List[A], rem: List[List[A]]): List[A] =
        rem match {
          case Nil => acc
          case Cons(x, xs) => x match {
            case Nil => go(acc, xs)
            case Cons(y, ys) => go(appendElem(acc, y), Cons(tail(x), xs))
          }
        }
      go(Nil, l)
    }

    def map[A, B](l: List[A])(f: A => B): List[B] = l match {
      case Nil => Nil
      case Cons(x, xs) => Cons(f(x), map(xs)(f))
    }

    def addOne(l: List[Int]): List[Int] = l match {
      case Nil => l
      case Cons(x, xs) => Cons(x + 1, addOne(xs))
    }

    def addOneUsingMap(l: List[Int]): List[Int] =
      List.map[Int, Int](l)(i => i + 1)

    def stringifyDoubles(l: List[Double]): List[String] =
      List.map(l)(_.toString)

    def filter[A](l: List[A])(predicate: A => Boolean): List[A] = l match {
      case Nil => l
      case Cons(x, xs) =>
        if (predicate(x)) Cons(x, filter(xs)(predicate))
        else filter(xs)(predicate)
    }

    def flatMap[A, B](l: List[A])(f: A => List[B]): List[B] = l match {
      case Nil => Nil
      case Cons(x, xs) => append(f(x), flatMap(xs)(f))
    }

    def filterUsingFlatMap[A](l: List[A])(f: A => Boolean): List[A] =
      List.flatMap(l)(e => if (f(e)) Cons(e, Nil) else Nil)

    def addElements(l1: List[Int], l2: List[Int]): List[Int] =
      l1 match {
        case Nil => l2
        case Cons(x, xs) => l2 match {
          case Nil => l1
          case Cons(y, ys) => Cons(x + y, addElements(xs, ys))
        }
      }

    def computeLists[A](l1: List[A], l2: List[A])(f: (A, A) => A): List[A] =
      l1 match {
        case Nil => l2
        case Cons(x, xs) => l2 match {
          case Nil => l1
          case Cons(y, ys) => Cons(f(x, y), computeLists(xs, ys)(f))
        }
      }

    def hasSubsequence[A](l: List[A], sub: List[A]): Boolean =
      sub match {
        case Nil => true
        case Cons(x, xs) =>
          l match {
            case Nil => false
            case Cons(y, ys) =>
              if (x == y) hasSubsequence(ys, xs)
              else hasSubsequence(ys, sub)
          }
      }


  }

  sealed trait Tree[+A]

  case class Leaf[A](value: A) extends Tree[A]

  case class Branch[A](left: Tree[A], right: Tree[A]) extends Tree[A]

  object Tree {
    def size[A](t: Tree[A]): Int = t match {
      case Leaf(v) => 1
      case Branch(l, r) => 1 + size(l) + size(r)
    }

    def maximum(t: Tree[Int]): Int = t match {
      case Leaf(v) => v
      case Branch(l, r) => maximum(l).max(maximum(r))
    }

    def depth[A](t: Tree[A]): Int = t match {
      case Leaf(v) => 0
      case Branch(l, r) => 1 + depth(l).max(depth(r))
    }

    def map[A, B](t: Tree[A])(f: A => B): Tree[B] =
      t match {
        case Leaf(v) => Leaf(f(v))
        case Branch(l, r) => Branch(map(l)(f), map(r)(f))
      }

    def fold[A, B](t: Tree[A], init: A => B)(f: (Tree[A], Tree[A]) => B): B =
      t match {
        case Leaf(v) => init(v)
        case Branch(l, r) => f(l, r)
      }

    def sizeUsingFold[A](t: Tree[A]): Int =
      fold[A, Int](t, _ => 1)((l, r) => 1 + size(l) + size(r))

    def maximumUsingFold(t: Tree[Int]): Int =
      fold[Int, Int](t, v => v)((l, r) => maximum(l).max(maximum(r)))

    def depthUsingFold[A](t: Tree[A]): Int =
      fold[A, Int](t, _ => 0)((l, r) => 1 + depth(l).max(depth(r)))

    def mapUsingFold[A, B](t: Tree[A])(f: A => B): Tree[B] =
      fold[A, Tree[B]](t, v => Leaf(f(v)))((l, r) => Branch(map(l)(f), map(r)(f)))
  }

}

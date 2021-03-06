package com.mle.fpis

/**
 * @author Michael
 */
object Trees {

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

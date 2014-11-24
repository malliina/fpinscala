package com.mle.fpis

import com.mle.fpis.Monads.Functor

/**
 * @author Michael
 */
object Applicatives {

  trait Applicative[F[_]] extends Functor[F] {
    // primitives
    def map2[A, B, C](fa: F[A], fb: F[B])(f: (A, B) => C): F[C]

    def unit[A](a: => A): F[A]

    // derived
    def apply[A, B](fab: F[A => B])(fa: F[A]): F[B] = map2(fab, fa)((ab, a) => ab(a))

    def map2InTermsOfApply[A, B, C](fa: F[A], fb: F[B])(f: (A, B) => C): F[C] = {
      val abc: A => B => C = f.curried
      val bc = map(fa)(abc)
      apply(bc)(fb)
    }

    def map[A, B](fa: F[A])(f: A => B): F[B] =
      apply[A, B](unit(f))(fa)

    def applyInTermsOfMap2[A, B](fab: F[A => B])(fa: F[A]): F[B] = map2(fab, fa)((ab, a) => ab(a))

    def mapUsingMap2[A, B](fa: F[A])(f: A => B): F[B] = map2(fa, unit(()))((a, _) => f(a))

    def traverse[A, B](as: List[A])(f: A => F[B]): F[List[B]] =
      as.foldRight(unit(List.empty[B]))((a, acc) => map2(f(a), acc)(_ :: _))

    def sequence[A](fas: List[F[A]]): F[List[A]] = traverse(fas)(fa => fa)

    def replicateM[A](n: Int, fa: F[A]): F[List[A]] = traverse((1 to n).toList)(_ => fa)

    def product[A, B](fa: F[A], fb: F[B]): F[(A, B)] = map2(fa, fb)((_, _))

    def map3[A, B, C, D](fa: F[A], fb: F[B], fc: F[C])(f: (A, B, C) => D): F[D] = {
      val abcd = f.curried
      val fbcd = map(fa)(abcd)
      apply(apply(fbcd)(fb))(fc)
    }

    def map4[A, B, C, D, E](fa: F[A], fb: F[B], fc: F[C], fd: F[D])(f: (A, B, C, D) => E): F[E] = {
      val fbcde: F[B => C => D => E] = map(fa)(f.curried)
      apply(apply(apply(fbcde)(fb))(fc))(fd)
    }
  }

}

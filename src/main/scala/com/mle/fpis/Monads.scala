package com.mle.fpis

import com.mle.fpis.Applicatives.Applicative
import com.mle.fpis.MyParser.Parser
import com.mle.fpis.Par.Par

/**
 * @author Michael
 */
object Monads {

  trait Functor[F[_]] {
    def map[A, B](fa: F[A])(f: A => B): F[B]

    def distribute[A, B](fab: F[(A, B)]): (F[A], F[B]) = (map(fab)(_._1), map(fab)(_._2))
  }

  trait Monad[F[_]] extends Applicative[F] {
    override def map2[A, B, C](fa: F[A], fb: F[B])(f: (A, B) => C): F[C] =
      flatMap(fa)(a => map(fb)(b => f(a, b)))

    def flatMap[A, B](fa: F[A])(f: A => F[B]): F[B]

    override def map[A, B](fa: F[A])(f: (A) => B): F[B] = flatMap(fa)(a => unit(f(a)))

    //    def sequence2[A](lma: List[F[A]]): F[List[A]] =
    //      lma.foldRight(unit(List.empty[A]))((fa, acc) => map2(fa, acc)(_ :: _))

    //    def traverse2[A, B](la: List[A])(f: A => F[B]): F[List[B]] =
    //      sequence2(la map f)

    def filterM[A](as: List[A])(f: A => F[Boolean]): F[List[A]] = as match {
      case Nil => unit(Nil)
      case h :: t => flatMap(f(h))(b => map(filterM(t)(f))(rest => if (b) h :: rest else rest))
    }

    def compose[A, B, C](f: A => F[B], g: B => F[C]): A => F[C] =
      a => flatMap(f(a))(g)

    def join[A](ffa: F[F[A]]): F[A] = ???
  }

  val listFunctor = new Functor[List] {
    override def map[A, B](fa: List[A])(f: (A) => B): List[B] = fa map f
  }
  val genMonad = new Monad[Gen] {
    override def unit[A](a: => A): Gen[A] = Gen.unit(a)

    override def flatMap[A, B](fa: Gen[A])(f: (A) => Gen[B]): Gen[B] = fa.flatMap(f)
  }
  val parMonad = new Monad[Par] {
    override def unit[A](a: => A): Par[A] = Par.unit(a)

    override def flatMap[A, B](fa: Par[A])(f: (A) => Par[B]): Par[B] = Par.flatMap(fa)(f)
  }
  val parserMonad = new Monad[Parser] {
    override def unit[A](a: => A): Parser[A] = ???

    override def flatMap[A, B](fa: Parser[A])(f: (A) => Parser[B]): Parser[B] = ???
  }
  val optionMonad = new Monad[Option] {
    override def unit[A](a: => A): Option[A] = Option(a)

    override def flatMap[A, B](fa: Option[A])(f: (A) => Option[B]): Option[B] = fa flatMap f
  }
  val streamMonad = new Monad[Stream] {
    override def unit[A](a: => A): Stream[A] = Stream(a)

    override def flatMap[A, B](fa: Stream[A])(f: (A) => Stream[B]): Stream[B] = fa flatMap f
  }
  val listMonad = new Monad[List] {
    override def unit[A](a: => A): List[A] = List(a)

    override def flatMap[A, B](fa: List[A])(f: (A) => List[B]): List[B] = fa flatMap f
  }

  case class Id[A](value: A) {
    def map[B](f: A => B): Id[B] = Id(f(value))

    def flatMap[B](f: A => Id[B]): Id[B] = f(value)
  }

  val idMonad = new Monad[Id] {
    override def unit[A](a: => A): Id[A] = Id(a)

    override def flatMap[A, B](fa: Id[A])(f: A => Id[B]): Id[B] = fa flatMap f
  }

  def stateMonad[S] = new Monad[({type f[x] = State[S, x]})#f] {
    override def unit[A](a: => A): State[S, A] = State(s => (a, s))

    override def flatMap[A, B](fa: State[S, A])(f: A => State[S, B]): State[S, B] = fa flatMap f
  }

  val intStateMonad = stateMonad[Int]
  val rngStateMonad = stateMonad[RNG]

  case class Reader[R, A](run: R => A)

  object Reader {
    def readerMonad[R] = new Monad[({type f[x] = Reader[R, x]})#f] {
      override def unit[A](a: => A): Reader[R, A] = Reader(r => a)

      override def flatMap[A, B](fa: Reader[R, A])(f: A => Reader[R, B]): Reader[R, B] =
        Reader(r => {
          val a = fa run r
          f(a) run r
        })
    }
  }

}

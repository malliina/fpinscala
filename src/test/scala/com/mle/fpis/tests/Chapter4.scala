package com.mle.fpis.tests


/**
 *
 * @author mle
 */
object Chapter4 {

  case class Some[+A](get: A) extends Option[A]

  case object None extends Option[Nothing]

  // E4.1
  trait Option[+A] {
    def map[B](f: A => B): Option[B] = this match {
      case None => None
      case Some(value) => Some(f(value))
    }

    def flatMap[B](f: A => Option[B]): Option[B] = this match {
      case None => None
      case Some(value) => f(value)
    }

    def getOrElse[B >: A](default: => B): B = this match {
      case None => default
      case Some(value) => value
    }

    def orElse[B >: A](ob: => Option[B]): Option[B] = this match {
      case None => ob
      case other => other
    }

    def filter(f: A => Boolean): Option[A] =
      flatMap(a => if (f(a)) Some(a) else None)
  }

  def mean(xs: Seq[Double]): Option[Double] =
    if (xs.isEmpty) None
    else Some(xs.sum / xs.length)

  // E4.2
  // variance is the mean of math.pow(x-m,2)
  def variance(xs: Seq[Double]): Option[Double] =
    mean(xs).flatMap(m => mean(xs.map(x => math.pow(x - m, 2))))

  // E4.3
  def map2[A, B, C](a: Option[A], b: Option[B])(f: (A, B) => C): Option[C] =
    a flatMap (aValue => b.map(bValue => f(aValue, bValue)))

  // E4.4
  def sequence[A](a: List[Option[A]]): Option[List[A]] = a match {
    case Nil => Some(Nil)
    case h :: t => h.flatMap(head => sequence(t).map(tail => head :: tail))
  }

  // E4.5
  def traverse[A, B](a: List[A])(f: A => Option[B]): Option[List[B]] = a match {
    case Nil => Some(Nil)
    case h :: t => f(h).flatMap(b => traverse(t)(f).map(list => b :: list))
  }

  def sequenceInTermsOfTraverse[A](a: List[Option[A]]): Option[List[A]] =
    traverse(a)(aOpt => aOpt)

  def Try[A](a: => A): Either[Exception, A] =
    try Right(a)
    catch {
      case e: Exception => Left(e)
    }

  //  sealed trait Either[+E, +A]
  case class Left[+E](value: E) extends Either[E, Nothing]

  case class Right[+A](value: A) extends Either[Nothing, A]

  trait Either[+E, +A] {
    def map[B](f: A => B): Either[E, B] =
      flatMap(v => Right(f(v)))

    def flatMap[EE >: E, B](f: A => Either[EE, B]): Either[EE, B] = this match {
      case Left(e) => Left(e)
      case Right(v) => f(v)
    }

    def orElse[EE >: E, B >: A](b: => Either[EE, B]): Either[EE, B] = this match {
      case Left(e) => b
      case other => other
    }

    def map2[EE >: E, B, C](b: Either[EE, B])(f: (A, B) => C): Either[EE, C] =
      flatMap(aValue => b.map(bValue => f(aValue, bValue)))


  }

  object Either {
    def traverse[A, B, E](a: List[A])(f: A => Either[E, B]): Either[E, List[B]] = a match {
      case Nil => Right(Nil)
      case h :: t => f(h).flatMap(b => traverse(t)(f).map(list => b :: list))
    }

    def sequenceInTermsOfTraverse[E, A](a: List[Either[E, A]]): Either[E, List[A]] =
      traverse(a)(aOpt => aOpt)
  }

}

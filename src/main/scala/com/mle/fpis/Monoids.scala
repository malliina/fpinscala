package com.mle.fpis

import com.mle.fpis.Trees.Tree

/**
 * @author Michael
 */
object Monoids {

  trait Monoid[A] {
    def op(a1: A, a2: A): A

    def zero: A
  }


  val stringMonoid = new Monoid[String] {
    override def op(a1: String, a2: String): String = a1 + a2

    override def zero: String = ""
  }

  def listConcatMonoid[A] = new Monoid[List[A]] {
    override def op(a1: List[A], a2: List[A]): List[A] = a1 ++ a2

    override def zero: List[A] = Nil
  }

  val intAddition = new Monoid[Int] {
    override def op(a1: Int, a2: Int): Int = a1 + a2

    override def zero: Int = 0
  }
  val intMultiplication = new Monoid[Int] {
    override def op(a1: Int, a2: Int): Int = a1 * a2

    override def zero: Int = 1
  }
  val booleanOr = new Monoid[Boolean] {
    override def op(a1: Boolean, a2: Boolean): Boolean = a1 || a2

    override def zero: Boolean = false
  }
  val booleanAnd = new Monoid[Boolean] {
    override def op(a1: Boolean, a2: Boolean): Boolean = a1 && a2

    override def zero: Boolean = true
  }

  def optionMonoid[A] = new Monoid[Option[A]] {
    override def op(a1: Option[A], a2: Option[A]): Option[A] = a1 orElse a2

    override def zero: Option[A] = None
  }

  def endoMonoid[A] = new Monoid[A => A] {
    override def op(a1: (A) => A, a2: (A) => A): (A) => A = a => a2(a1(a))

    override def zero: (A) => A = a => a
  }

  def monoidLaws[A](m: Monoid[A], gen: Gen[A]): Prop = monoidIdentityLaw(m, gen) && monoidAssocLaw(m, gen)

  def monoidIdentityLaw[A](m: Monoid[A], gen: Gen[A]): Prop =
    Prop.forAll(gen)(a => m.op(a, m.zero) == a && m.op(m.zero, a) == a)

  def monoidAssocLaw[A](m: Monoid[A], gen: Gen[A]): Prop =
    Prop.forAll(Gen.listOfN(3, gen))(list => {
      val List(x, y, z) = list
      m.op(m.op(x, y), z) == m.op(x, m.op(y, z))
    })

  def trimMonoid(s: String): Monoid[String] = new Monoid[String] {
    override def op(a1: String, a2: String): String = {
      val separator = if ((a1 endsWith " ") || (a2 startsWith " ")) "" else " "
      s"$a1$separator$a2".trim
    }

    override def zero: String = ""
  }

  def concatenate[A](as: List[A], m: Monoid[A]): A = as.fold(m.zero)(m.op)

  def foldMap[A, B](as: List[A], m: Monoid[B])(f: A => B): B = concatenate(as map f, m)

  def foldRightUsingFoldMap[A, B](as: List[A])(init: B)(f: (A, B) => B): B = {
    val monoid = endoMonoid[B]
    foldMap(as, monoid)(f.curried)(init)
  }

  def foldMapV[A, B](v: IndexedSeq[A], m: Monoid[B])(f: A => B): B = {
    val size = v.size
    size match {
      case 0 => m.zero
      case 1 => f(v.head)
      case i =>
        val (left, right) = v splitAt size / 2
        m.op(foldMapV(left, m)(f), foldMapV(right, m)(f))
    }
  }

  // must use foldMap
  def isSorted(seq: IndexedSeq[Int]): Boolean = {
    val pairs = seq zip seq.tail
    foldMap(pairs.toList, booleanAnd)(pair => pair._1 <= pair._2)
  }

  // word count
  sealed trait WC

  case class Stub(chars: String) extends WC

  case class Part(leftStub: String, words: Int, rightStub: String) extends WC

  val wcMonoid = new Monoid[WC] {
    override def op(a1: WC, a2: WC): WC = a1 match {
      case Stub(chars) => a2 match {
        case Stub(chars2) => Stub(chars + chars2)
        case Part(left, words, right) => Part(chars + left, words, right)
      }
      case Part(left, words, right) => a2 match {
        case Stub(chars2) => Part(left, words, right + chars2)
        case Part(left2, words2, right2) =>
          val wordIncrement = if ((right + left2).nonEmpty) 1 else 0
          Part(left, words + words2 + wordIncrement, right2)
      }
    }

    override def zero: WC = Stub("")
  }

  // must use wcMonoid
  def wordCount(s: String): Int = {
    foldWords(concatenate(toWCs(s), wcMonoid))
  }

  def toWCs(s: String): List[WC] = {
    val split = s.size > 10
    if (split) {
      val (left, right) = s splitAt s.size / 2
      toWCs(left) ++ toWCs(right)
    } else {
      List(count(s))
    }
  }

  def count(s: String): WC = ???

  def foldWords(count: WC) = count match {
    case Stub(_) => 1
    case Part(l, words, r) => (if (l.nonEmpty) 1 else 0) + words + (if (r.nonEmpty) 1 else 0)
  }

  trait Foldable[F[_]] {
    def foldRight[A, B](as: F[A])(init: B)(f: (A, B) => B): B

    def foldLeft[A, B](as: F[A])(init: B)(f: (B, A) => B): B

    def foldMap[A, B](as: F[A])(f: A => B)(mb: Monoid[B]): B

    def concatenate[A](as: F[A])(m: Monoid[A]): A = foldLeft(as)(m.zero)(m.op)

    def toList[A](fa: F[A]): List[A] = foldLeft(fa)(List.empty[A])((acc, a) => a :: acc)
  }

  val listFoldable = new Foldable[List] {
    override def foldRight[A, B](as: List[A])(init: B)(f: (A, B) => B): B = as.foldRight(init)(f)

    override def foldLeft[A, B](as: List[A])(init: B)(f: (B, A) => B): B = as.foldLeft(init)(f)

    override def foldMap[A, B](as: List[A])(f: (A) => B)(mb: Monoid[B]): B = (as map f).fold(mb.zero)(mb.op)
  }

  val indexedSeqFoldable = new Foldable[IndexedSeq] {
    override def foldRight[A, B](as: IndexedSeq[A])(init: B)(f: (A, B) => B): B = as.foldRight(init)(f)

    override def foldLeft[A, B](as: IndexedSeq[A])(init: B)(f: (B, A) => B): B = as.foldLeft(init)(f)

    override def foldMap[A, B](as: IndexedSeq[A])(f: (A) => B)(mb: Monoid[B]): B = (as map f).fold(mb.zero)(mb.op)
  }

  val treeFoldable = new Foldable[Trees.Tree] {
    override def foldRight[A, B](as: Tree[A])(init: B)(f: (A, B) => B): B = ???

    override def foldLeft[A, B](as: Tree[A])(init: B)(f: (B, A) => B): B = ???

    override def foldMap[A, B](as: Tree[A])(f: (A) => B)(mb: Monoid[B]): B = ???
  }

  val optionFoldable = new Foldable[Option] {
    override def foldRight[A, B](as: Option[A])(init: B)(f: (A, B) => B): B = as.foldRight(init)(f)

    override def foldLeft[A, B](as: Option[A])(init: B)(f: (B, A) => B): B = as.foldLeft(init)(f)

    override def foldMap[A, B](as: Option[A])(f: (A) => B)(mb: Monoid[B]): B = (as map f).foldLeft(mb.zero)(mb.op)
  }

  def productMonoid[A, B](a: Monoid[A], b: Monoid[B]): Monoid[(A, B)] = new Monoid[(A, B)] {
    override def op(a1: (A, B), a2: (A, B)): (A, B) = {
      val (a1a, a1b) = a1
      val (a2a, a2b) = a2
      (a.op(a1a, a2a), b.op(a1b, a2b))
    }

    override def zero: (A, B) = (a.zero, b.zero)
  }

  def eitherMonoid[A, B](a: Monoid[A], b: Monoid[B]): Monoid[Either[A, B]] = ???

  def mapMergeMonoid[K, V](v: Monoid[V]): Monoid[Map[K, V]] =
    new Monoid[Map[K, V]] {
      override def op(a1: Map[K, V], a2: Map[K, V]): Map[K, V] = {
        val pairsInA1 = a1 map {
          case (key, value) => (key, v.op(value, a2.getOrElse(key, v.zero)))
        }
        // also includes pairs that are exclusively in a2
        val pairsOnlyInA2 = a2.filterKeys(key => !a1.contains(key)).map {
          case (key, value) => (key, v.op(value, v.zero))
        }
        pairsInA1 ++ pairsOnlyInA2
      }

      override def zero: Map[K, V] = Map()
    }

  def functionMonoid[A, B](b: Monoid[B]): Monoid[A => B] = new Monoid[A => B] {
    override def op(a1: A => B, a2: A => B): A => B = (a: A) => b.op(a1(a), a2(a))

    override def zero: A => B = a => b.zero
  }

  def bag[A](as: IndexedSeq[A]): Map[A, Int] = {
    val addMonoid = mapMergeMonoid[A, Int](intAddition)
    foldMapV(as, addMonoid)(a => Map(a -> 1))
    //    as.foldLeft(Map.empty[A, Int])((acc, a) => {
    //      acc.updated(a, acc.getOrElse(a, 0) + 1)
    //    })
  }
}

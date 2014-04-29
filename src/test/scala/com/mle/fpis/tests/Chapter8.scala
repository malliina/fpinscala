package com.mle.fpis.tests

import com.mle.fpis.tests.Chapter8.Prop.{SuccessCount, FailedCase}
import com.mle.fpis.tests.Chapter6.{RandF, State, RNG, intf}

/**
 *
 * @author mle
 */
object Chapter8 {


  //  trait Prop {
  //    def check: Either[(FailedCase, SuccessCount), SuccessCount]
  //  }


  case class Gen[A](sample: State[RNG, A]) {
    def flatMap[B](f: A => Gen[B]): Gen[B] =
      Gen(sample.flatMap(a => f(a).sample))

    def listOfN(size: Gen[Int]): Gen[List[A]] =
      size.flatMap(Gen.listOfN(_, this))
  }

  object Gen {
    def choose(start: Int, stopExclusive: Int): Gen[Int] =
      Gen(State(between(start, stopExclusive)))

    def unit[A](a: => A): Gen[A] = Gen(State.unit(a))

    def boolean: Gen[Boolean] = Gen(State(intf).map(_ < 0))

    def listOfN[A](n: Int, g: Gen[A]): Gen[List[A]] =
      Gen(State(Chapter6.sequence(List.fill(n)(g.sample.run))))

    def sameParity(from: Int, to: Int): Gen[(Int, Int)] =
      choose(from, to + 1).flatMap(first => {
        val predicate: Int => Boolean = if (first % 2 == 0) _ % 2 == 0 else _ % 2 != 0
        Gen(State(having(predicate)).map(second => (first, second)))
      })

    def union[A](g1: Gen[A], g2: Gen[A]): Gen[A] =
      boolean.flatMap(b => if (b) g1 else g2)

    def weighted[A](g1: (Gen[A], Double), g2: (Gen[A], Double)): Gen[A] = {
      val (ga, weightA) = g1
      val (gb, weightB) = g2
      val g1Share = weightA / (weightA + weightB)
      choose(0, Integer.MAX_VALUE).flatMap(value => {
        if (value < g1Share * Integer.MAX_VALUE) ga else gb
      })
    }
  }

  private def between(start: Int, stopExclusive: Int): RandF[Int] = rng => {
    val (i, state) = rng.nextInt
    if (i >= start && i < stopExclusive) (i, state)
    else between(start, stopExclusive)(state)
  }

  private def having(p: Int => Boolean): RandF[Int] = rng => {
    val (i, state) = rng.nextInt
    if (p(i)) (i, state)
    else having(p)(state)
  }

  object Prop {
    type FailedCase = String
    type SuccessCount = Int
    //    def forAll[A](as:Gen[A])(f: A => Boolean):Prop = Prop((n,rng) => {
    //      randomStream(as)(rng).zip(Stream.from(0)).take(n).map {
    //        case (a, i) => try {
    //          if (f(a)) None else Some((a.toString, i))
    //        } catch { case e: Exception => Some((buildMsg(a, e), i)) }
    //      }.find(_.isDefined).getOrElse(None)
    //    })
  }

  type TestCases = Int
  type Result = Option[(FailedCase, SuccessCount)]

  //  private def list[A](count: Int, init: State[RNG, A]): S
  case class Prop(run: (TestCases, RNG) => Result) {
    def &&(p: Prop): Prop = logic((thisResult, remainingTests, rng) => {
      if (thisResult.isEmpty) p.run(remainingTests, rng)
      else thisResult
    })

    def ||(p: Prop): Prop = logic((thisResult, remainingTests, rng) => {
      if (thisResult.isEmpty) thisResult
      else p.run(remainingTests, rng)
    })

    private def logic(f: (Result, TestCases, RNG) => Result): Prop = Prop((n, rng) => {
      val half = n / 2
      val incr = if (half + half < n) 1 else 0
      val thisResult = run(half, rng)
      f(thisResult, half + incr, rng)
    })
  }

}

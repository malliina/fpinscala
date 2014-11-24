package com.mle.fpis

/**
 * @author Michael
 */
case class SGen[A](forSize: Int => Gen[A])

object SGen {
  def listOf[A](g: Gen[A]): SGen[List[A]] = SGen(i => Gen.listOfN(i, g))

  def listOf1[A](g: Gen[A]): SGen[List[A]] = SGen(i => Gen.listOfN(i + 1, g))
}
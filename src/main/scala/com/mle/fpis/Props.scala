package com.mle.fpis

import java.util.concurrent.Executors

import com.mle.fpis.Prop._

/**
 *
 * @author mle
 */
object ** {
  def unapply[A, B](p: (A, B)) = Some(p)
}

object Props {

  val smallInt = Gen.choose(-10, 10)
  val maxProp = forAll(SGen.listOf1(smallInt))(list => {
    val max = list.max
    !list.exists(_ > max)
  })

  val sortProp = forAll(SGen.listOf(smallInt))(list => {
    isSorted(list.sorted)
  })

  def isSorted(list: List[Int]): Boolean = list match {
    case f :: s :: t => f <= s && isSorted(s :: t)
    case _ => true
  }

  val ES = Executors.newCachedThreadPool()
  val p2 = check {
    val p = Par.map(Par.unit(1))(_ + 1)
    val p2 = Par.unit(2)
    p(ES).get() == p2(ES).get()
  }
  val p22 = check {
    Par.equal(Par.map(Par.unit(1))(_ + 1), Par.unit(2))(ES).get()
  }
  val p222 = checkPar(Par.equal(Par.map(Par.unit(1))(_ + 1), Par.unit(2)))

  val pint = Gen.choose(0, 10) map Par.unit
  val p4 = forAllPar(pint)(n => Par.equal(Par.map(n)(y => y), n))

  val forkProperty = forAllPar(pint)(parUnit => Par.equal(Par.fork(parUnit), parUnit))
}

package tests

import com.mle.fpis.Monads.Id
import com.mle.fpis.{Monads, Monoids, RNG, State}
import org.scalatest.FunSuite

/**
 * @author Michael
 */
class MonoidsMonads extends FunSuite {
  test("Monoids.isSorted(IndexedSeq)") {
    assert(Monoids.isSorted(IndexedSeq(1, 2, 3, 4, 5)))
    assert(!Monoids.isSorted(IndexedSeq(4, 2, 3, 4, 5)))
  }
  test("Monoids.bag") {
    val seq = IndexedSeq("a", "b", "a", "d")
    val expected = Map("a" -> 2, "b" -> 1, "d" -> 1)
    assert(Monoids.bag(seq) === expected)
  }
  test("Monoids.productMonoid") {
    import com.mle.fpis.Monoids._
    val m = productMonoid(intAddition, intAddition)
    val (count, sum) = listFoldable.foldMap(List(1, 2, 3, 4))(a => (1, a))(m)
    assert(count === 4)
    assert(sum === 10)
  }
  test("ID Monad") {
    assert(Id("hoi").value === "hoi")
    val helloMonad = Id("Hello, ") flatMap (h => Id("Monad!") flatMap (m => Id(h + m)))
    assert(helloMonad.value === "Hello, Monad!")
    val helloMonad2 =
      for {
        h <- Id("Hello, ")
        m <- Id("Monad!")
      } yield h + m
    assert(helloMonad2 === helloMonad)
  }
  test("State monad") {
    val m = Monads.rngStateMonad
    val rng = new RNG.Simple(42)
    val rngState = State[RNG, Int](_.nextInt)
    val replicatedState = m.replicateM(5, rngState)
    val (intList, _) = replicatedState.run(rng)
  }
}

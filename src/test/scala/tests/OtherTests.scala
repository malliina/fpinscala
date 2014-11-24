package tests

import org.scalatest.FunSuite

/**
 * @author Michael
 */
class OtherTests extends FunSuite {
  test("list deconstruction") {
    val List(x, y, z) = List(1, 2, 3)
    assert(x === 1 && y === 2 && z === 3)
    assert(x === 1)
  }
}

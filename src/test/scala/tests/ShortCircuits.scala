package tests

import com.mle.fpis.Streams.Stream
import org.scalatest.FunSuite

/**
 * @author Michael
 */
class ShortCircuits extends FunSuite {
  test("foldLeft multiplication") {
    def printed(num: Int) = {
      println(num)
      num
    }
    println("Folding stream")
    val testStream = Stream(1, 2, 0, 3) map printed
    val res = testStream.foldRight(1)((elem, acc) => elem * acc)
    assert(res === 0)
    0 * printed(400)
  }
}

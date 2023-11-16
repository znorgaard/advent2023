package advent
package days
package advent
import org.scalatest.funsuite.AnyFunSuite

class DayOneTests extends AnyFunSuite {

  // test 1
  test("'double' should handle 0") {
    val result = DayOne.double(0)
    assert(result == 0)
  }

  // test 2
  test("'double' should handle 1") {
    val result = DayOne.double(1)
    assert(result == 2)
  }

  test("test with Int.MaxValue")(pending)

}

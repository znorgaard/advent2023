package advent
package template
import org.scalatest.funsuite.AnyFunSuite
class DayTemplateTests extends AnyFunSuite {

  // test 1
  test("'double' should handle 0") {
    val result = DayTemplate.double(0)
    assert(result == 0)
  }

  // test 2
  test("'double' should handle 1") {
    val result = DayTemplate.double(1)
    assert(result == 2)
  }

  test("test with Int.MaxValue")(pending)

}
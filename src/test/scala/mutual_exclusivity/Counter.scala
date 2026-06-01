package mutual_exclusivity

import language.experimental.captureChecking
import language.experimental.separationChecking
import caps.unsafe._

import boc._

def count(n: Int): Cown[Option[Int]] = {
  if (n == 1) return Cown(Some(1))

  when(count(n - 1)) { last =>
    last.get.get + 1
  }
}

// For more information on writing tests, see
// https://scalameta.org/munit/docs/getting-started.html
class CountTest extends munit.FunSuite {
  test("Count to 100") {
    val n: Int = 1000
    val actual: Cown[Option[Int]] = count(n)

    blockingWhen (actual) { actual =>
      assertEquals(n, actual.get.get)
    }
  }
}

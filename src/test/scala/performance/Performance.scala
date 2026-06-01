package performance

import boc._

class PerformanceTest extends munit.FunSuite {
  test("") {
    for (_ <- 0 until 100000) {
      when {
        var i = 0
        for (_ <- 0 until 500000) { i += 1 }
      }
    }
    Behaviour.awaitall
  }
}

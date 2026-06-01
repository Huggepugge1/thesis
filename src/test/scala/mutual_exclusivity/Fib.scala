package mutual_exclusivity

import language.experimental.captureChecking
import language.experimental.separationChecking

import boc._

def fib(n: Int): Int = {
  if (n <= 2) return 1
  fib(n - 1) + fib(n - 2)
}

def fib_boc(n: Int, result: Cown[Int]): Unit = {
  if (n <= 2) {
    when(result) { result => result.set(1) }
    return
  }
  val a = Cown(0)
  val b = Cown(0)
  fib_boc(n - 1, a)
  fib_boc(n - 2, b)
  when(a, b, result) { (a, b, result) =>
    result.set(a.get + b.get)
  }
}

def fib_boc(n: Int): Cown[Option[Int]] = {
  if (n <= 2) return Cown(Some(1))
  when(fib_boc(n - 1), fib_boc(n - 2)) { (f1, f2) =>
    f1.get.get + f2.get.get
  }
}

def annoying = {
  var value = 0
  when {
    println(value)
  }
  value = 42
}

// def fib_boc_other(n: Int, result: Cown[Int], barrier: Barrier): Unit = {
//   if (n <= 2) {
//     when()(barrier)(result) { result => result.set(1) }
//     return
//   }
//
//   val a = Cown(0)
//   val b = Cown(0)
//   val a_barrier = Barrier()
//   val b_barrier = Barrier()
//   when { fib_boc_other(n - 1, a, a_barrier) }
//   when { fib_boc_other(n - 2, b, b_barrier) }
//   when(a_barrier, b_barrier)(barrier)(a, b, result) { (a, b, result) =>
//     result.set(a.get + b.get)
//   }
// }

class FibTest extends munit.FunSuite {
  test("Fibonacci sequence with result") {
    val n = 15
    for (i <- 0 to 50) {
      val expected = fib(n)
      val actual = Cown(0)
      fib_boc(n, actual)

      blockingWhen(actual) { actual =>
        println(s"${expected} | ${actual.get}")
        // assertEquals(expected, actual_value)
      }
    }
  }

  test("Fibonacci sequence") {
    val n = 15
    for (i <- 0 to 50) {
      val expected = fib(n)
      val actual = fib_boc(n)

      blockingWhen(actual) { actual =>
        assertEquals(expected, actual.get.get)
      }
    }
  }

  test("annoying") {
    annoying
  }

  // test("Fibonacci sequence other") {
  //   val n = 5
  //   for (i <- 0 to 0) {
  //     val expected = fib(i)
  //     val actual = Cown(0)
  //     fib_boc_other(n, actual)
  //
  //     Behaviour.awaitall
  //
  //     when(actual) { actual =>
  //       println(actual.get)
  //       // assertEquals(expected, actual.get)
  //     }
  //   }
  // }
}

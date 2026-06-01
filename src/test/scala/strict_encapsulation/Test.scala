// package strict_encapsulation
//
// import language.experimental.captureChecking
// import language.experimental.separationChecking
//
// import scala.caps.Mutable
//
// import scala.concurrent.Future
// import scala.concurrent.ExecutionContext.Implicits.global
//
// import caps.unsafe._
//
// import scala.collection.mutable.ArrayBuffer
//
// import boc._
//
// class A(var inner: Int) extends Mutable {
//   update def +=(other: Int) = this.inner += other
//   def get: Int = this.inner
// }
//
// def createCown[T](data: T): Cown[T] = Cown(data)
//
// class SETest extends munit.FunSuite {
//   test("Two cowns") {
//     var a: A^ = A(0)
//
//     {
//       val cown: TrackedCown[A^{}, {}]^ = TrackedCown(a)
//       when(cown) { (cown) =>
//         cown.get += 1
//       }
//     }
//     {
//       val cown: TrackedCown[A^{}, {}]^ = TrackedCown(a)
//       when(cown) { cown =>
//         cown.get += 1
//       }
//     }
//   }
// }

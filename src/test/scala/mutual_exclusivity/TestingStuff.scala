package mutual_exclusivity

import java.io.{OutputStream, FileOutputStream}

import language.experimental.captureChecking
import language.experimental.separationChecking

import scala.caps.unsafe.unsafeAssumePure
import scala.caps.unsafe.untrackedCaptures

import scala.collection.mutable.ArrayBuffer

import boc._

class TestingStuff extends munit.FunSuite {
  test("Bool") {
    var boolean: Boolean = true
    val cown = Cown(boolean)

    when(cown) { acquired  =>
      val boolean = acquired.get
      when {
        boolean
      }
    }
    Behaviour.awaitall
  }

  test("Java Bool - Should not compile because not pure") {
    var boolean: java.lang.Boolean = true
    val cown = Cown(boolean)

    when(cown) { acquired =>
      val boolean = acquired.get
      // acquired.get
    }

    Behaviour.awaitall
  }

  test("Wrapper of tracked - Should NOT compile") {
    class B
    class A(init: B) {
      @untrackedCaptures var b: B^ = init
    }
 
    val a = A(B())
    val cown = Cown(a)
    var b = B()

    when(cown) { acquired =>
      acquired.get.b = b
      // acquired.get.b
    }
    Behaviour.awaitall
  }

  test("Seq of Cowns") {
    class A(init: Integer) extends caps.ExclusiveCapability {
      val i: Integer^ = init
    }
    var cowns: ArrayBuffer[Cown[A]] = ArrayBuffer()

    var a = A(0)

    cowns += Cown(a)
    cowns += Cown(A(1))
    cowns += Cown(A(2))
    cowns += Cown(A(3))

    when(cowns.toSeq*) { acquired_cowns =>
      for (i <- acquired_cowns) {
        // println(i.get.i)
      }
      acquired_cowns(0).get.i
    }
    Behaviour.awaitall
  }
}

class A(val string: String)

// def fun[T](op: OutputStream^ ->{caps.any.rd} T): T

// def usingFile[T](name: String, op: OutputStream^ -> T): T = {
//     val f = new FileOutputStream(name)
//     val result = op(f)
//     f.close()
//     result
// }
//
// usingFile("hello.txt", f => () => f.write('h'))

def restrictive_function[T](
    op: ->{caps.any.rd} T
): T = {
    op
}

def wow = {
  var a = 0
  // Ok
  restrictive_function { a }

  // Not ok
  // restrictive_function { a = 5 }
}

// def does_not_compile = {
//   var cown: CapabilityCown[Int] = CapabilityCown(0)
//   var cown2: CapabilityCown[Int] = cown
//
//   // Separation error:
//   // "cown" and "cown2" are the same
//   when(cown, cown2) { (acq, _) =>
//     acq.set(acq.get + 100)
//   }
// }

def what: Unit = {
  class A

  var buf = ArrayBuffer[A^]()
  val a: A^ = A()
  buf += a
  buf += a
}

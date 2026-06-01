package mutual_exclusivity

import boc._
import language.experimental.captureChecking
import language.experimental.separationChecking

import scala.caps._
import scala.caps.unsafe.{untrackedCaptures, unsafeAssumePure}

class Account extends Mutable {
  var balance = 100
  var frozen = false
}

def basic_transfer(
  src: Cown[Account^],
  dst: Cown[Account^],
  amount: Int): Unit = {
  when (src) { src => src.get.balance -= amount }
  when (dst) { dst => dst.get.balance += amount }
}

def nested_transfer(
  src: Cown[Account^]^,
  dst: Cown[Account^]^,
  amount: Int): Unit = {
  val dst_rd: Cown[Account^]^{dst.rd} = dst.rd

  when(src) { src =>
    if (src.get.balance >= amount) {
      src.get.balance -= amount
      when (dst_rd) { dst => dst.get.balance += amount }
    }
  }
}

def nested_transfer_capability(
  src: CapabilityCown[Account^],
  dst: CapabilityCown[Account^],
  amount: Int): Unit = {
  val dst_rd = dst.rd

  when(src) { src =>
    if (src.get.balance >= amount) {
      src.get.balance -= amount
      when (dst_rd) { dst => dst.get.balance += amount }
    }
  }
}

def require_both_accounts_transfer(
  src: Cown[Account^]^,
  dst: Cown[Account^]^,
  amount: Int): Unit = {
    when (src, dst) { (src, dst) =>
      if (src.get.balance >= amount
        && !src.get.frozen
        && !dst.get.frozen) {
          src.get.balance -= amount
          dst.get.balance += amount
        }
    }
}

class AccountTest extends munit.FunSuite {
  test("Basic Transfer") {
    val acc: Account = Account()
    var acc1 = Cown(Account())
    var acc2 = Cown(Account())

    basic_transfer(acc1, acc2, 100)
    basic_transfer(acc1, acc2, 100)

    Behaviour.awaitall
    blockingWhen (acc1, acc2) { (acc1, acc2) =>
      assertEquals(acc1.get.balance, -100)
      assertEquals(acc2.get.balance, 300)
    }
  }

  test("Nested Transfer") {
    var acc1: Cown[Account^] = Cown(Account())
    var acc2: Cown[Account^] = Cown(Account())
    nested_transfer(acc1, acc2, 100)
    nested_transfer(acc1, acc2, 100)
    Behaviour.awaitall
    blockingWhen (acc1, acc2) { (acc1, acc2) =>
      assertEquals(acc1.get.balance, 0)
      assertEquals(acc2.get.balance, 200)
    }
  }

  test("Require Both Accounts Transfer") {
    var acc1: Cown[Account^] = Cown(Account())
    var acc2: Cown[Account^] = Cown(Account())
    require_both_accounts_transfer(acc1, acc2, 100)
    require_both_accounts_transfer(acc1, acc2, 100)

    Behaviour.awaitall
    blockingWhen (acc1, acc2) { (acc1, acc2) =>
      assertEquals(acc1.get.balance, 0)
      assertEquals(acc2.get.balance, 200)
    }
  }

  test("Require Both Accounts Transfer Order") {
    {
      var acc1 = Cown(Account())
      var acc2 = Cown(Account())
      // 0 200
      require_both_accounts_transfer(acc1, acc2, 100)
      // 200 0
      require_both_accounts_transfer(acc2, acc1, 200)

      Behaviour.awaitall
      blockingWhen (acc1, acc2) { (acc1, acc2) =>
        assertEquals(acc1.get.balance, 200)
        assertEquals(acc2.get.balance, 0)
      }
    }
    {
      var acc1 = Cown(Account())
      var acc2 = Cown(Account())

      // Fails -> 100 100
      require_both_accounts_transfer(acc2, acc1, 200)
      // 0 200
      require_both_accounts_transfer(acc1, acc2, 100)

      Behaviour.awaitall
      blockingWhen (acc1, acc2) { (acc1, acc2) =>
        assertEquals(acc1.get.balance, 0)
        assertEquals(acc2.get.balance, 200)
      }
    }
  }
  
  test("") {
    var acc = Account()
    var cown1: CapabilityCown[Account^] = CapabilityCown(acc)
    var cown2: CapabilityCown[Account^] = CapabilityCown(acc)

    when(cown1, cown2) { (acq, _) => acq.get.balance += 100 }
  }

  test("") {
    val cown1: CapabilityCown[Account^] = CapabilityCown(Account())
    val cown2: CapabilityCown[Account^] = CapabilityCown(Account())

    nested_transfer_capability(cown1, cown2, 100)
  }

  // test("leaky") {
  //   class A[T](var value: T)
  //   val option: Option[Int] = Some(50)
  //   val cown = Cown(A(option))
  //   var leaked: Int = 0
  //   var v: A[Option[Int]] = A(Some(1))
  //   when (cown) { acquired_cown =>
  //     leaked = acquired_cown.get.value.get
  //     acquired_cown.set(v) // Allowed!!
  //   }
  //   println(v)
  // }
}

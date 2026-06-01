package boc

import language.experimental.captureChecking
import scala.caps.{SharedCapability, ExclusiveCapability, Mutable}
import scala.caps.unsafe.{untrackedCaptures, unsafeAssumePure}

import scala.util.{Success, Failure}
import scala.concurrent.Future
import scala.concurrent.ExecutionContext.Implicits.global

import java.util.concurrent.atomic.{AtomicInteger, AtomicReference}
import java.util.concurrent.CountDownLatch

var nextId = AtomicInteger(0)

// import scala.concurrent.{ExecutionContext, Future}
// import java.util.concurrent.ForkJoinPool

// val fjp = new ForkJoinPool(16)
// val ec = ExecutionContext.fromExecutorService(fjp)
// implicit val executionContext: ExecutionContext = ec

def unreachable: Unit = assert(false, "Reached unreachable code!")

class Value[T](initial: T) {
  var value: T = initial
}

class Cown[T](initial: T) {
  val last: AtomicReference[Request[T]] = AtomicReference(null)
  val id = nextId.getAndIncrement

  @volatile
  private[boc] var value: Value[T] = Value(initial)

  private[boc] def acquire: AcquiredCown[T]^ = {
    AcquiredCown(this.value)
  }

  def rd: Cown[T]^{this.rd} = unsafeAssumePure(this)
}

class CapabilityCown[T](initial: T) extends Cown(initial), ExclusiveCapability

class AcquiredCown[T](private var value: Value[T]) extends Mutable {
  update def set(new_value: T): Unit = this.value.value = new_value
  update def get: T^{this} = this.value.value
}

class Request[T](@untrackedCaptures target: Cown[T]) extends caps.Pure {
  @volatile var scheduled = false
  @volatile var next: Option[Behaviour[?]] = None

  def startAppendReq(behaviour: Behaviour[?]) = {
    var prev = this.target.last.getAndSet(this)
    prev match {
      case null => behaviour.resolveOne
      case prev => {
        while (
          !prev.scheduled
        ) {}
        prev.next = Some(behaviour)
      }
    }
  }

  def finishAppendReq: Unit = this.scheduled = true

  var called_released = 0
  def release: Unit = {
    called_released += 1
    if (this.next == None) {
      if (this.target.last.compareAndSet(this, null)) return
      while (this.next == None) {
        /* Short spin lock until next is done scheduling */
      }
    }
    this.next match {
      case None       => unreachable
      case Some(next) => next.resolveOne
    }
  }
}

def make_requests(cowns: Seq[Cown[?]]): Seq[Request[?]] = {
  cowns.map(Request(_))
}

class Behaviour[T](
  @untrackedCaptures cowns: Seq[Cown[?]],
  @untrackedCaptures thunk: => T,
  @untrackedCaptures result: Cown[Option[T]])
  extends caps.Pure {

  val requests: Seq[Request[?]] = make_requests(cowns)

  // Number of resolves needed to start the behaviour
  var count = AtomicInteger(cowns.length + 1)

  def resolveOne: Unit = {
    if (this.count.decrementAndGet != 0) {
      return
    }

    Future {
      this.result.value.value = Some(this.thunk)
      this.requests.foreach(_.release)
      Behaviour.behaviour_count.decrementAndGet
    }
  }
}

object Behaviour {
  var behaviour_count = AtomicInteger(0)
  var total_spawned = AtomicInteger(0)

  def schedule[T](cowns: Seq[Cown[?]], thunk: => T): Cown[Option[T]] = {
    // assert(all(cowns) not same)
    this.behaviour_count.incrementAndGet
    this.total_spawned.incrementAndGet

    // Sort to prevent deadlock
    given Ordering[Cown[?]] = Ordering.by(_.id)
    val result: Cown[Option[T]] = Cown(None)
    val sorted_cowns: Seq[Cown[?]] = cowns.sorted ++ Seq(result)

    // Why is this ok to be val????
    val behaviour: Behaviour[T] = Behaviour(sorted_cowns, thunk, result)
    behaviour.requests.foreach(_.startAppendReq(behaviour))
    behaviour.requests.foreach(_.finishAppendReq)
    behaviour.resolveOne
    result
  }

  /**
   * CAUTION: CANNOT BE USED INSIDE ANOTHER BEHAVIOUR
   * Usage of this function does NOT guarantee deadlock-free execution!
   */
  def awaitall = {
    // FIX:
    // Spin lock is not a good implementation
    while (Behaviour.behaviour_count.get != 0) {}
  }
}

/**
 * CAUTION: CANNOT BE USED INSIDE ANOTHER BEHAVIOUR
 * Usage of this function does NOT guarantee deadlock-free execution!
 */
def blockingWhenSchedule[T](cowns: Seq[Cown[?]], thunk: => T): Cown[Option[T]] = {
  // NOTE: Question to them, why sort?
  given Ordering[Cown[?]] = Ordering.by(_.id)
  val result: Cown[Option[T]] = Cown(None)
  val sorted_cowns: Seq[Cown[?]] = cowns.sorted ++ Seq(result)

  val behaviour: Behaviour[T] = Behaviour(sorted_cowns, thunk, result)
  behaviour.requests.foreach(_.startAppendReq(behaviour))
  behaviour.requests.foreach(_.finishAppendReq)

  while (behaviour.count.get != 1) {}

  // Fake resolve
  behaviour.count.decrementAndGet

  result.value.value = Some(thunk)
  behaviour.requests.foreach(_.release)
  result
}

package boc

import language.experimental.captureChecking
import caps.unsafe.unsafeAssumePure

/**
 * CAUTION: CANNOT BE USED INSIDE ANOTHER BEHAVIOUR
 * Usage of this function does NOT guarantee deadlock-free execution!
 */
def blockingWhen[T](thunk: ->{caps.any.rd} T): Cown[Option[T]] = {
  blockingWhenSchedule(
    Seq(),
    thunk,
  )
}

/**
 * CAUTION: CANNOT BE USED INSIDE ANOTHER BEHAVIOUR
 * Usage of this function does NOT guarantee deadlock-free execution!
 */
def blockingWhen[
  T,
  C, CC^,
](
  cown: Cown[C^{CC}]^,
)(thunk:
  AcquiredCown[C^{CC}]^ ->{caps.any.rd} T): Cown[Option[T]] = {
  blockingWhenSchedule(
    Seq(
      unsafeAssumePure(cown),
    ),
    thunk(
      cown.acquire,
    ),
  )
}

/**
 * CAUTION: CANNOT BE USED INSIDE ANOTHER BEHAVIOUR
 * Usage of this function does NOT guarantee deadlock-free execution!
 */
def blockingWhen[
  T,
  C1, CC1^,
  C2, CC2^,
](
  cown1: Cown[C1^{CC1}],
  cown2: Cown[C2^{CC2}],
)(thunk:
  (
    AcquiredCown[C1^{CC1}]^,
    AcquiredCown[C2^{CC2}]^,
  ) ->{caps.any.rd} T): Cown[Option[T]] = {
  blockingWhenSchedule(
    Seq(
      unsafeAssumePure(cown1),
      unsafeAssumePure(cown2),
    ),
    thunk(
      cown1.acquire,
      cown2.acquire,
    ),
  )
}

/**
 * CAUTION: CANNOT BE USED INSIDE ANOTHER BEHAVIOUR
 * Usage of this function does NOT guarantee deadlock-free execution!
 */
def blockingWhen[
  T,
  C1,
  C2,
  C3,
](
  cown1: Cown[C1],
  cown2: Cown[C2],
  cown3: Cown[C3],
)(thunk:
  (
    AcquiredCown[C1]^,
    AcquiredCown[C2]^,
    AcquiredCown[C3]^,
  ) ->{caps.any.rd} T): Cown[Option[T]] = {
  blockingWhenSchedule(
    Seq(
      cown1,
      cown2,
      cown3,
    ),
    thunk(
      cown1.acquire,
      cown2.acquire,
      cown3.acquire,
    ),
  )
}

/**
 * CAUTION: CANNOT BE USED INSIDE ANOTHER BEHAVIOUR
 * Usage of this function does NOT guarantee deadlock-free execution!
 */
def blockingWhen[
  T,
  C1,
  C2,
  C3,
  C4,
](
  cown1: Cown[C1],
  cown2: Cown[C2],
  cown3: Cown[C3],
  cown4: Cown[C4],
)(thunk:
  (
    AcquiredCown[C1]^,
    AcquiredCown[C2]^,
    AcquiredCown[C3]^,
    AcquiredCown[C4]^,
  ) ->{caps.any.rd} T): Cown[Option[T]] = {
  blockingWhenSchedule(
    Seq(
      cown1,
      cown2,
      cown3,
      cown4,
    ),
    thunk(
      cown1.acquire,
      cown2.acquire,
      cown3.acquire,
      cown4.acquire,
    ),
  )
}

/**
 * CAUTION: CANNOT BE USED INSIDE ANOTHER BEHAVIOUR
 * Usage of this function does NOT guarantee deadlock-free execution!
 */
def blockingWhen[
  T,
  C1,
  C2,
  C3,
  C4,
  C5,
](
  cown1: Cown[C1],
  cown2: Cown[C2],
  cown3: Cown[C3],
  cown4: Cown[C4],
  cown5: Cown[C5],
)(thunk:
  (
    AcquiredCown[C1]^,
    AcquiredCown[C2]^,
    AcquiredCown[C3]^,
    AcquiredCown[C4]^,
    AcquiredCown[C5]^,
  ) ->{caps.any.rd} T): Cown[Option[T]] = {
  blockingWhenSchedule(
    Seq(
      cown1,
      cown2,
      cown3,
      cown4,
      cown5,
    ),
    thunk(
      cown1.acquire,
      cown2.acquire,
      cown3.acquire,
      cown4.acquire,
      cown5.acquire,
    ),
  )
}

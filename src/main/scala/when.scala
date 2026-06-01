package boc

import language.experimental.captureChecking
import language.experimental.separationChecking

import caps.unsafe.unsafeAssumePure

def when[T](thunk: ->{caps.any.rd} T): Cown[Option[T]] = {
  Behaviour.schedule(
    Seq(),
    thunk,
  )
}

def when[
  T,
  C, CC^,
](
  cown: Cown[C^{CC}]^,
)(thunk: AcquiredCown[C^{CC}]^ ->{caps.any.rd} T): Cown[Option[T]] = {
  Behaviour.schedule(
    Seq(
      unsafeAssumePure(cown),
    ),
    thunk(
      cown.acquire,
    ),
  )
}

def when[
  T,
  C1, CC1^,
  C2, CC2^,
](
  cown1: Cown[C1^{CC1}]^,
  cown2: Cown[C2^{CC2}]^,
)(thunk:
  (
    AcquiredCown[C1^{CC1}]^,
    AcquiredCown[C2^{CC2}]^,
  ) ->{caps.any.rd} T): Cown[Option[T]] = {
  Behaviour.schedule(
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

def when[
  T,
  C1, CC1^,
  C2, CC2^,
  C3, CC3^,
](
  cown1: Cown[C1^{CC1}]^,
  cown2: Cown[C2^{CC2}]^,
  cown3: Cown[C3^{CC3}]^,
)(thunk:
  (
    AcquiredCown[C1^{CC1}]^,
    AcquiredCown[C2^{CC2}]^,
    AcquiredCown[C3^{CC3}]^,
  ) ->{caps.any.rd} T): Cown[Option[T]] = {
  Behaviour.schedule(
    Seq(
      unsafeAssumePure(cown1),
      unsafeAssumePure(cown2),
      unsafeAssumePure(cown3),
    ),
    thunk(
      cown1.acquire,
      cown2.acquire,
      cown3.acquire,
    ),
  )
}

def when[
  T,
  C1, CC1^,
  C2, CC2^,
  C3, CC3^,
  C4, CC4^,
](
  cown1: Cown[C1^{CC1}]^,
  cown2: Cown[C2^{CC2}]^,
  cown3: Cown[C3^{CC3}]^,
  cown4: Cown[C4^{CC4}]^,
)(thunk:
  (
    AcquiredCown[C1^{CC1}]^,
    AcquiredCown[C2^{CC2}]^,
    AcquiredCown[C3^{CC3}]^,
    AcquiredCown[C4^{CC4}]^,
  ) ->{caps.any.rd} T): Cown[Option[T]] = {
  Behaviour.schedule(
    Seq(
      unsafeAssumePure(cown1),
      unsafeAssumePure(cown2),
      unsafeAssumePure(cown3),
      unsafeAssumePure(cown4),
    ),
    thunk(
      cown1.acquire,
      cown2.acquire,
      cown3.acquire,
      cown4.acquire,
    ),
  )
}

def when[
  T,
  C1, CC1^,
  C2, CC2^,
  C3, CC3^,
  C4, CC4^,
  C5, CC5^,
](
  cown1: Cown[C1^{CC1}]^,
  cown2: Cown[C2^{CC2}]^,
  cown3: Cown[C3^{CC3}]^,
  cown4: Cown[C4^{CC4}]^,
  cown5: Cown[C5^{CC5}]^,
)(thunk:
  (
    AcquiredCown[C1^{CC1}]^,
    AcquiredCown[C2^{CC2}]^,
    AcquiredCown[C3^{CC3}]^,
    AcquiredCown[C4^{CC4}]^,
    AcquiredCown[C5^{CC5}]^,
  ) ->{caps.any.rd} T): Cown[Option[T]] = {
  Behaviour.schedule(
    Seq(
      unsafeAssumePure(cown1),
      unsafeAssumePure(cown2),
      unsafeAssumePure(cown3),
      unsafeAssumePure(cown4),
      unsafeAssumePure(cown5),
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

def acquire_cown[C, CC^](cown: Cown[C]): AcquiredCown[C]^{CC} = unsafeAssumePure(cown.acquire)

def when[
  T,
  C,
  CC^,
  CC1^,
](
  cowns: (Cown[C^{CC}])*,
)(thunk:
  (acquired_cowns: Seq[AcquiredCown[C^{CC}]^{CC1}])
  ->{caps.any.rd}
  T
): Cown[Option[T]] = {
  Behaviour.schedule(
    cowns,
    thunk(
      cowns.map(acquire_cown(_))
    ),
  )
}

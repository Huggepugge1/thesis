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

def leaky = {
  var value: Int = 0
  when { value = 42 }
  println(value) // 0 | 42
}

// def leaky = {
//   val cown = Cown(42)
//   when (cown) { outer => 
//     when (cown) { inner => 
//       println(outer.get)
//     }
//   }
// }

// def when[T, C](
//   cown: Cown[C]
// )(
//   thunk: AcquiredCown[C]^ ->{caps.any.rd} T
// ): Cown[Option[T]] = {
//   Behaviour.schedule(
//     Seq(
//       cown,
//     ),
//     thunk(
//       cown.acquire,
//     )
//   )
// }

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
      unsafeAssumePure(cown1.acquire),
      unsafeAssumePure(cown2.acquire),
    ),
  )
}

def when[
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
  Behaviour.schedule(
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

def when[
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
  Behaviour.schedule(
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

def when[
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
  Behaviour.schedule(
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

def acquire_cown[C, CC^](cown: Cown[C]): AcquiredCown[C]^{CC} = unsafeAssumePure(cown.acquire)

// def whenSeq[
//   T,
//   C,
//   CC^,
//   CC1^,
// ](
//   cowns: Seq[Cown[C^{CC}]],
// )(thunk:
//   (acquired_cowns: Seq[AcquiredCown[C^{CC}]^{CC1}])
//   ->{caps.any.rd}
//   T
// ): Cown[Option[T]] = {
//   Behaviour.schedule(
//     cowns,
//     thunk(
//       cowns.map(acquire_cown(_))
//     ),
//   )
// }

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

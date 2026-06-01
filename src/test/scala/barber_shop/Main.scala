package barber_shop

import boc._
import language.experimental.captureChecking
import language.experimental.separationChecking

class BarberTest extends munit.FunSuite {
  test("Spawn One") {
    val n = 1

    val barber = Barber()
    val barber_cown: Cown[Barber^] = Cown(barber)
    val waiting_room = Cown(WaitingRoom(n))
    customer(barber_cown, waiting_room)

    Behaviour.awaitall
  }

  test("Spawn Two") {
    val n = 2

    val barber = Cown(Barber())
    val waiting_room = Cown(WaitingRoom(n))
    customer(barber, waiting_room)
    customer(barber, waiting_room)

    Behaviour.awaitall
  }

  test("Spawn 10") {
    val n = 10

    val barber = Cown(Barber())
    val waiting_room = Cown(WaitingRoom(n))
    for (i <- 0 until n) customer(barber, waiting_room)

    Behaviour.awaitall
  }

  test("Spawn 1000") {
    val n = 1000

    val barber = Cown(Barber())
    val waiting_room = Cown(WaitingRoom(n))
    for (i <- 0 until n) customer(barber, waiting_room)

    Behaviour.awaitall
  }

  test("Spawn 1000 and then spawn more slowly") {
    val n = 1000

    val barber = Cown(Barber())
    val waiting_room = Cown(WaitingRoom(n))
    for (i <- 0 until n) customer(barber, waiting_room)
    for (i <- 0 until n) {
      Thread.sleep(1)
      customer(barber, waiting_room)
    }

    Behaviour.awaitall
  }

  test("Cut 10, fall asleep, and spawn 10 more") {
    val n = 10

    val barber = Cown(Barber())
    val waiting_room = Cown(WaitingRoom(n))
    for (i <- 0 until n) customer(barber, waiting_room)

    Thread.sleep(1000)

    for (i <- 0 until n) customer(barber, waiting_room)

    Behaviour.awaitall
  }
}

package dining_philosopers

import language.experimental.captureChecking
import language.experimental.separationChecking

import boc._

import scala.collection.mutable.ArrayBuffer

class Fork

class Few extends munit.FunSuite {
  test("2 philosophers") {
    val f1 = Cown(Fork())
    val f2 = Cown(Fork())
    when (f1, f2) {(_, _) => println(1)}
    when (f2, f1) {(_, _) => println(2)}
  }

  test("4 philosophers") {
    val f1 = Cown(Fork())
    val f2 = Cown(Fork())
    val f3 = Cown(Fork())
    val f4 = Cown(Fork())
    when (f1, f2) {(_, _) => println(1)}
    when (f2, f3) {(_, _) => println(2)}
    when (f3, f4) {(_, _) => println(3)}
    when (f4, f1) {(_, _) => println(4)}
  }

  test("4 philosophers - out of order") {
    val f1 = Cown(Fork())
    val f2 = Cown(Fork())
    val f3 = Cown(Fork())
    val f4 = Cown(Fork())
    when (f1, f2) {(_, _) => println(1)}
    when (f3, f4) {(_, _) => println(3)}
    when (f2, f3) {(_, _) => println(2)}
    when (f4, f1) {(_, _) => println(4)}
  }
}

class Many extends munit.FunSuite {
  val n = 100
  val m = 250
  val WORK_SLEEP = 1
  val IN_ORDER = false
  val PRINT = false
  val SLEEP = false
  val WORK = true

  def eat(i: Int) = {
    if (PRINT && i == 0) when {println(s"${i} is eating")}
    if (SLEEP) Thread.sleep(WORK_SLEEP)
    if (WORK) {
      val duration = 100L // 0.1ms
      val start = System.nanoTime()
      val finish = start + duration * 1000L

      while (System.nanoTime() < finish) {}
    }
  }

  if (IN_ORDER) {
    test(s"${n} philosophers - in order") {
      val forks = ArrayBuffer[Cown[Fork]]()

      for (i <- 1 to n) {
        forks += Cown(Fork())
      }

      for (_ <- 0 until m) {
        for (i <- 0 until n) {
          when (forks(i), forks((i + 1) % n)) {(_, _) => eat(i)}
        }
        Behaviour.awaitall
      }
    }
  } else {
    test(s"${n} philosophers - Every other") {
      val forks = ArrayBuffer[Cown[Fork]]()

      for (i <- 0 until n) {
        forks += Cown(Fork())
      }

      for (_ <- 0 until m) {
        for (i <- 0 until n by 2) {
          when (forks(i), forks((i + 1) % n)) {(_, _) => eat(i)}
        }
        for (i <- 1 until n by 2) {
          when (forks(i), forks((i + 1) % n)) {(_, _) => eat(i)}
        }
        Behaviour.awaitall
      }
      println(Behaviour.total_spawned)
    }
  }
}

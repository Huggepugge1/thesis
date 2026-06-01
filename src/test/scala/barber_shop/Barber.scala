package barber_shop

import boc._
import language.experimental.captureChecking
import language.experimental.separationChecking

import scala.caps.unsafe.untrackedCaptures

class Barber extends scala.caps.Mutable {
  var asleep: Boolean = true
  var cut_hairs: Int = 0

  update def wake = this.asleep = false
  update def cut[C^](barber: Cown[Barber^], waiting_room: Cown[WaitingRoom]): Unit = {
    this.cut_hairs += 1

    barber_wait(this, waiting_room)

    Thread.sleep(2)

    class A extends caps.ExclusiveCapability
    val a = A()

    val cown1 = Cown(a)
    val cown2 = Cown(a)

    next(barber, waiting_room)
  }
}

def barber_wait(barber: Barber, waiting_room: Cown[WaitingRoom]^): Unit = {
  when(waiting_room) { (acq_waiting_room) =>
    acq_waiting_room.get.free += 1
    print(s"\rWaiting room: ${acq_waiting_room.get.chairs - acq_waiting_room.get.free}/${acq_waiting_room.get.chairs}, hairs cut: ${barber.cut_hairs}")
  }
}

def next(barber: Cown[Barber^], waiting_room: Cown[WaitingRoom]): Unit = {
  when(barber, waiting_room) { (acq_barber, acq_waiting_room) =>
    if (acq_waiting_room.get.free == acq_waiting_room.get.chairs) {
      acq_barber.get.asleep = true
    } else {
      acq_barber.get.cut(barber, waiting_room)
    }
  }
}

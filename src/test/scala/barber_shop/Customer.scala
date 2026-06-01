package barber_shop

import boc._
import language.experimental.captureChecking
import language.experimental.separationChecking

def customer[C](barber: Cown[Barber^], waiting_room: Cown[WaitingRoom]) = {
  when(waiting_room) { acq_waiting_room =>
    if (acq_waiting_room.get.has_free) {
      // The room is now has one more customer, until the barber cuts hair
      acq_waiting_room.get.free -= 1

      when(barber) { acq_barber =>
        // The room is now has one less customer
        if (acq_barber.get.asleep) {
          println("\nWoke up barber")
          acq_barber.get.wake
          acq_barber.get.cut(barber, waiting_room)
        }
      }
    }
  }
}

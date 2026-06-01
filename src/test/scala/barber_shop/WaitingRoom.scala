package barber_shop

import boc._
import language.experimental.captureChecking
import language.experimental.separationChecking
import scala.caps.unsafe.untrackedCaptures

class WaitingRoom(val chairs: Int) {
  @untrackedCaptures var free = chairs

  def has_free = free >= 0
}

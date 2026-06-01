package cigarette_smokers

import language.experimental.captureChecking
import language.experimental.separationChecking
import scala.caps.unsafe.untrackedCaptures

class Table {
  @untrackedCaptures var tobacco: Boolean = false
  @untrackedCaptures var paper: Boolean = false
  @untrackedCaptures var _match: Boolean = false
  @untrackedCaptures var done: Boolean = false

  def empty = !(this.tobacco || this.paper || this._match)
  def tobacco_ready = this.paper && this._match
  def paper_ready = this.tobacco && this._match
  def match_ready = this.tobacco && this.paper
  def clear = {
    this.tobacco = false
    this.paper = false
    this._match = false
  }
}

package cigarette_smokers

import language.experimental.separationChecking

import boc._

class Agent(rng: scala.util.Random) {
  def place(table_cown: Cown[Table], n: Int): Unit = {
    if (n == 0) {
      when(table_cown) { table => table.get.done = true }
      return
    }

    when(table_cown) { table =>
      if (table.get.empty) {
        val random = rng.nextInt(3)
        if (random == 0) {
          if (DEBUG) {
            println("Ready for match")
          }
          table.get.tobacco = true
          table.get.paper = true
        } else if (random == 1) {
          if (DEBUG) {
            println("Ready for tobacco")
          }
          table.get.paper = true
          table.get._match = true
        } else if (random == 2) {
          if (DEBUG) {
            println("Ready for paper")
          }
          table.get.tobacco = true
          table.get._match = true
        }
        this.place(table_cown, n - 1)
      } else {
        this.place(table_cown, n)
      }
    }
  }
}

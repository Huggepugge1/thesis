package cigarette_smokers

import boc._
import language.experimental.captureChecking
import language.experimental.separationChecking

import scala.caps.unsafe.untrackedCaptures

enum Ingredient {
  case Tobacco
  case Paper
  case Match
}

object Smoker {
  @untrackedCaptures var smoked = 0
  @untrackedCaptures var checked = 0
}

class Smoker(val ingredient: Ingredient) {
  @untrackedCaptures var smoked = 0
  def smoke(table: Table^) = {
    table.clear
    Smoker.smoked += 1
    this.smoked += 1
    if (DEBUG) {
      println(s"Smoker ${this.ingredient} smoking")
      println(s"Smoking progress: ${Smoker.smoked}/${n}")
    } else if (Smoker.smoked % (n / 100) == 0) {
      print(s"\rSmoking progress: ${Smoker.smoked}/${n}")
    }
    // Thread.sleep(1)
  }

  def try_smoke(table_cown: Cown[Table], cown: Cown[Any]): Unit = {
    Smoker.checked += 1
    when(table_cown) { (table: AcquiredCown[Table^{}]^) =>
     this.ingredient match {
      case Ingredient.Tobacco => if (table.get.tobacco_ready) this.smoke(table.get)
      case Ingredient.Paper   => if (table.get.paper_ready)   this.smoke(table.get)
      case Ingredient.Match   => if (table.get.match_ready)   this.smoke(table.get)
      }

      if (!table.get.done) {
        this.try_smoke(table_cown, cown)
      }
    }
  }
}

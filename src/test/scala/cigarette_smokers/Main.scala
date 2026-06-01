package cigarette_smokers

import language.experimental.separationChecking

import boc._

val n = 100000
val DEBUG = false

class CigaretteTest extends munit.FunSuite {
  test(s"${n} smokes") {
    val table = Cown(Table())

    val agent = Agent(scala.util.Random(0))
    agent.place(table, n)

    val tobacco_smoker = Smoker(Ingredient.Tobacco)
    val paper_smoker   = Smoker(Ingredient.Paper)
    val match_smoker   = Smoker(Ingredient.Match)

    tobacco_smoker.try_smoke(table, Cown(0))
    paper_smoker.try_smoke(table, Cown(0))
    match_smoker.try_smoke(table, Cown(0))

    Behaviour.awaitall

    assertEquals(Smoker.smoked, n)
    val expected = n / 3
    val tolerance = n * 0.01 // Wiggle room

    assert(math.abs(tobacco_smoker.smoked - expected) < tolerance)
    assert(math.abs(paper_smoker.smoked - expected) < tolerance)
    assert(math.abs(match_smoker.smoked - expected) < tolerance)

    assertEquals(
      Smoker.smoked,
      tobacco_smoker.smoked + paper_smoker.smoked + match_smoker.smoked,
    )

    blockingWhen(table) { table =>
      assertEquals(table.get.tobacco, false)
      assertEquals(table.get.paper, false)
      assertEquals(table.get._match, false)
    }
    
    assert(math.abs(Smoker.checked - 3 * n) < math.max(10, n / 10000))
  }
}

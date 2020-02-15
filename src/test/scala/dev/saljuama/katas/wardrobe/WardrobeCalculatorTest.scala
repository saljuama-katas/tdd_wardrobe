package dev.saljuama.katas.wardrobe

import org.scalatest.matchers.must.Matchers
import org.scalatest.wordspec.AnyWordSpec


class WardrobeCalculatorTest extends AnyWordSpec with Matchers {

  trait Fixture {
    val availableSizes = Set(50, 75, 100, 120)
    val wardrobeCalculator = new WardrobeCalculator(availableSizes)
  }

  "Wardrobe calculator" can {

    "calculate fitting combination" when {

      "there are no combinations from available sizes that can fit the wall" must {
        Seq(49, 74, 99, 119).foreach { size ⇒
          s"return an empty list of options when size is $size" in new Fixture {
            wardrobeCalculator.calculateFittingCombinations(size) mustBe empty
          }
        }
      }

      val emptyCombination = Map(50 → 0, 75 → 0, 100 → 0, 120 → 0)
      def assertSizeProducesAtLeastTheCombination(size: Int)(selectedSizes: Map[Int, Int]): Unit = {
        s"return at least the combination $selectedSizes for the size $size" in new Fixture {
          wardrobeCalculator.calculateFittingCombinations(size) must contain(Combination(selectedSizes))
        }
      }

      "the size of the wall is exactly the size of one of the available sizes" must {
        assertSizeProducesAtLeastTheCombination(50)(emptyCombination + (50 → 1))
        assertSizeProducesAtLeastTheCombination(75)(emptyCombination + (75 → 1))
        assertSizeProducesAtLeastTheCombination(100)(emptyCombination + (100 → 1))
        assertSizeProducesAtLeastTheCombination(120)(emptyCombination + (120 → 1))
      }

      "the size of the wall is a multiple of one of the available sizes" must {
        assertSizeProducesAtLeastTheCombination(100)(emptyCombination + (50 → 2))
        assertSizeProducesAtLeastTheCombination(150)(emptyCombination + (75 → 2))
        assertSizeProducesAtLeastTheCombination(200)(emptyCombination + (100 → 2))
        assertSizeProducesAtLeastTheCombination(240)(emptyCombination + (120 → 2))
      }

      "the size of the wall is a sum of 2 different available sizes" must {
        assertSizeProducesAtLeastTheCombination(125)(emptyCombination + (50 → 1) + (75 → 1))
        assertSizeProducesAtLeastTheCombination(150)(emptyCombination + (50 → 1) + (100 → 1))
        assertSizeProducesAtLeastTheCombination(170)(emptyCombination + (50 → 1) + (120 → 1))
      }

      "the size of the wall is a sum of multiples of different available sizes" must {
        assertSizeProducesAtLeastTheCombination(200)(emptyCombination + (50 → 1) + (75 → 2))
        assertSizeProducesAtLeastTheCombination(250)(emptyCombination + (50 → 1) + (100 → 2))
        assertSizeProducesAtLeastTheCombination(290)(emptyCombination + (50 → 1) + (120 → 2))
      }

    }
  }
}

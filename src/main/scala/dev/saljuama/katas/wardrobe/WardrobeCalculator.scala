package dev.saljuama.katas.wardrobe

class WardrobeCalculator(val availableSizes: Set[Int]) {
  private val initialSelectedSizes = availableSizes.map { _ → 0 }.toMap
  private case class Combination(remainingWallSize: Int, selectedSizes: Map[Int, Int] = initialSelectedSizes)

  def calculateFittingCombinations(size: Int): Set[Map[Int, Int]] = calculateFittingCombinations(Combination(size))

  private def calculateFittingCombinations(combination: Combination): Set[Map[Int, Int]] = {
    def fitsInTheWall(remainingWallSize: Int, candidateElementSize: Int) = remainingWallSize - candidateElementSize >= 0

    if (combination.remainingWallSize == 0)
      Set(combination.selectedSizes)
    else
      availableSizes
        .filter { size ⇒ fitsInTheWall(combination.remainingWallSize, size) }
        .flatMap { size ⇒
          val updatedRemainingWallSize = combination.remainingWallSize - size
          val updatedSelectedSizes = combination.selectedSizes + (size → (combination.selectedSizes(size) + 1))
          calculateFittingCombinations(Combination(updatedRemainingWallSize, updatedSelectedSizes))
        }
  }

}

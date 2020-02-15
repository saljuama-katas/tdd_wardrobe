package dev.saljuama.katas.wardrobe

class WardrobeCalculator(val availableSizes: Set[Int]) {
  private val initialSelectedSizes = availableSizes.map { _ → 0 }.toMap
  private case class CombinationTreeNode(remainingWallSize: Int, selectedSizes: Map[Int, Int] = initialSelectedSizes)

  def calculateFittingCombinations(size: Int): Set[Combination] = calculateFittingCombinations(CombinationTreeNode(size))

  private def calculateFittingCombinations(combination: CombinationTreeNode): Set[Combination] = {
    def fitsInTheWall(remainingWallSize: Int, candidateElementSize: Int) = remainingWallSize - candidateElementSize >= 0

    if (combination.remainingWallSize == 0)
      Set(Combination(combination.selectedSizes))
    else
      availableSizes
        .filter { size ⇒ fitsInTheWall(combination.remainingWallSize, size) }
        .flatMap { size ⇒
          val updatedRemainingWallSize = combination.remainingWallSize - size
          val updatedSelectedSizes = combination.selectedSizes + (size → (combination.selectedSizes(size) + 1))
          calculateFittingCombinations(CombinationTreeNode(updatedRemainingWallSize, updatedSelectedSizes))
        }
  }
}

case class Combination(selectedSizes: Map[Int, Int])
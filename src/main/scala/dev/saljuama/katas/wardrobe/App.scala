package dev.saljuama.katas.wardrobe

object App {
  def main(args: Array[String]): Unit = {
    val wallSize = 250
    val availableSizesAndPrices = Map(50 → 59, 75 → 62, 100 → 90, 120 → 111)

    val wardrobeCalculator = new WardrobeCalculator(availableSizesAndPrices)
    val result = wardrobeCalculator.calculateFittingCombinations(wallSize)

    val printer = new WardrobeCombinationsFormatter(10)
    printer.printHeader(availableSizesAndPrices.keys.toList)
    result.foreach { printer.printRow }
  }
}

class WardrobeCombinationsFormatter(val cellWidth: Int = 7) {

  private def formatCell(element: Any): String = s"%${cellWidth}s".format(element)

  private def formatRow(rowElements: List[Any]): String = rowElements.map { formatCell }.mkString(" ")

  private def printSeparationLine(numberOfColumns: Int): Unit = {
    val tableWidth = numberOfColumns * (cellWidth + 1)
    println("-" * tableWidth)
  }


  def printHeader(headerColumns: List[Int]): Unit = {
    val headers = headerColumns
      .sorted
      .map { _.toString }
      .appended("Size")

    printSeparationLine(headerColumns.size + 1)
    println(formatRow(headers))
    printSeparationLine(headerColumns.size + 1)
  }

  def printRow(result: Combination): Unit = {
    val combinationTotalSize = result.selectedSizes.map { entry ⇒ entry._1 * entry._2 }.sum
    val values = result.selectedSizes.toList
      .sortBy { _._1 }
      .map { _._2 }
      .appended { combinationTotalSize }

    println(formatRow(values))
  }

}

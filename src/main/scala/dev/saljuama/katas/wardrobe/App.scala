package dev.saljuama.katas.wardrobe

object App {
  def main(args: Array[String]): Unit = {
    val wallSize = 250
    val availableSizes = Set(50, 75, 100, 120)

    val wardrobeCalculator = new WardrobeCalculator(availableSizes)
    val result = wardrobeCalculator.calculateFittingCombinations(wallSize)

    val printer = new WardrobeCombinationsFormatter(10)
    printer.printHeader(availableSizes)
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


  def printHeader(headerColumns: Set[Int]): Unit = {
    val headers = headerColumns.toList
      .sorted
      .map { _.toString }
      .appended("Size")

    printSeparationLine(headerColumns.size + 1)
    println(formatRow(headers))
    printSeparationLine(headerColumns.size + 1)
  }

  def printRow(result: Map[Int, Int]): Unit = {
    val combinationTotalSize = result.map { entry ⇒ entry._1 * entry._2 }.sum
    val values = result.toList
      .sortBy { _._1 }
      .map { _._2 }
      .appended { combinationTotalSize }

    println(formatRow(values))
  }

}

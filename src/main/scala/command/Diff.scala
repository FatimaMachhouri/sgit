package command

object Diff {

  /**
   *
   * @param list1 List[String]
   * @param list2 List[String]
   * @param index1 Int
   * @param index2 Int
   * @param acc Map[(Int, Int), Int]
   * @return Map[(Int, Int), Int]
   * list1 and list2 are the 2 contents we want to compare splitted by line
   * Pre-conditions : index1 = 0, index2 = 0 and acc = Map()
   * Permits to build the matrix of the most largest common sub-set of the 2 lists.
   * Each elem of the list1 is put in line and each elem of the list2 is put in column.
   * The return map contains the list of (line index, column index) -> value associated in the matrix
   */
  def mostLargestCommonSubSetMatrix(list1: List[String], list2: List[String], index1: Int, index2: Int, acc: Map[(Int, Int), Int]): Map[(Int, Int), Int] = {
    //We stop when we go through the 2 lists
    if (list1.length - 1 <= index1 && list2.length <= index2) acc

    else {
      val newIndex1 = if (index2 == list2.length) index1 + 1 else index1
      val newIndex2 = if (index2 == list2.length) 0 else index2

      //If we are in the first line
      if (newIndex1 == 0) {
        if (list1(newIndex1) == list2(newIndex2)) mostLargestCommonSubSetMatrix(list1, list2, newIndex1, newIndex2 + 1, acc + ((newIndex1, newIndex2) -> 1))
        else if (newIndex2 == 0) mostLargestCommonSubSetMatrix(list1, list2, newIndex1, newIndex2 + 1, acc + ((newIndex1, newIndex2) -> 0))
        else mostLargestCommonSubSetMatrix(list1, list2, newIndex1, newIndex2 + 1, acc + ((newIndex1, newIndex2) -> acc.get((newIndex1, newIndex2-1)).getOrElse(0)))
      }

      //If we are in the first column
      else if (newIndex2 == 0) {
        if (list1(newIndex1) == list2(newIndex2)) mostLargestCommonSubSetMatrix(list1, list2, newIndex1, newIndex2 + 1, acc + ((newIndex1, newIndex2) -> 1))
        else if (newIndex1 == 0) mostLargestCommonSubSetMatrix(list1, list2, newIndex1, newIndex2 + 1, acc + ((newIndex1, newIndex2) -> 0))
        else mostLargestCommonSubSetMatrix(list1, list2, newIndex1, newIndex2 + 1, acc + ((newIndex1, newIndex2) -> acc.get((newIndex1-1, newIndex2)).getOrElse(0)))
      }

      else { //Neither the first column nor the first line
        if (list1(newIndex1) == list2(newIndex2)) {
          val newValue = acc.get((newIndex1-1, newIndex2-1)).getOrElse(0) + 1
          mostLargestCommonSubSetMatrix(list1, list2, newIndex1, newIndex2 + 1, acc + ((newIndex1, newIndex2) -> newValue))
        }
        else {
          val previousValueLine = acc.get((newIndex1, newIndex2 - 1)).getOrElse(0)
          val previousValueColumn = acc.get( (newIndex1 - 1 , newIndex2) ).getOrElse(0)
          val maxValue = if (previousValueColumn > previousValueLine) previousValueColumn else previousValueLine
          mostLargestCommonSubSetMatrix(list1, list2, newIndex1, newIndex2 + 1, acc + ((newIndex1, newIndex2) -> maxValue))
        }
      }
    }
  }

}

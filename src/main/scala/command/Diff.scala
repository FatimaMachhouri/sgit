package command

import java.io.File
import scala.annotation.tailrec
import utils.Path.getFilesDirectory
import utils.FileIO.getContentFile
import utils.Hash.encryptThisString

object Diff {

  /**
   *
   * @param rootPath String
   * @return Map[String, List[String]
   * Returns a map of String -> List[String]
   * The string corresponds to the path of the modified files (modified between last stage and working tree)
   * The list of strings correponds to list of differences between the staged version and the working tree version (each list is associated to its file)
   */
  def diff(rootPath: String): Map[String, List[String]] = {

    //Get paths of modified files
    val filesInCurrentDirectory = getFilesDirectory(rootPath)
    val stageContent = getContentFile(rootPath + File.separator + ".sgit" + File.separator + "STAGE")
    val listHashAndFilesDirectory = filesInCurrentDirectory.map(file => List(encryptThisString(getContentFile(file)), file.replace(rootPath + File.separator, "")))
    val modifiedFiles = getModifiedFiles(rootPath, stageContent, filesInCurrentDirectory, listHashAndFilesDirectory)

    if (modifiedFiles.isEmpty) Map()
    else {
      //The working tree : The contentFilesDirectoryMap val contains file paths as keys and file contents corresponding as value
      val contentFilesDirectory = modifiedFiles.map(path => getContentFile(rootPath + File.separator + path))
      val contentFilesDirectoryMap = mapPathAndContent(modifiedFiles, contentFilesDirectory, Map())

      //The stage
      //Step 1 : We get the files (hash and path) that are modified in the STAGE
      val stageContent = getContentFile(rootPath + File.separator + ".sgit" + File.separator + "STAGE")
      val stagedFilesModified = stageContent.split("\n").filter(elem => modifiedFiles.contains(elem.split(" ")(2)))

      //Step 2 : For each file, we retrieve the content by using hash and blobs
      val filesHash = stagedFilesModified.map(elem => elem.split(" ")(1))
      val stagedFilesContent = filesHash.map(hash => getContentFile(rootPath + File.separator + ".sgit" + File.separator + "Blobs" + File.separator + hash))

      //Step 3 : The stagedFilesMap val contains file paths as keys and file contents corresponding as value
      val stagedFilesMap = mapPathAndContent(stagedFilesModified.map(elem => elem.split(" ")(2)), stagedFilesContent, Map())

      //We merge the maps : The merged map contains the path as key and the 2 contents as value (content of the working tree file and of the staged file)
      val mergedMap = mergeMaps(contentFilesDirectoryMap, stagedFilesMap, Map())

      //For each file (key), we get the list of differences
      val differences = mergedMap.values.map(elem => {
        val text1 = elem(1).split("\n").toList
        val text2 = elem(0).split("\n").toList
        val matrix = mostLargestCommonSubSetMatrix(text1, text2, 0, 0, Map())
        getDifferences(text1, text2, text1.length - 1, text2.length - 1, matrix, List())
      })

      //Last step : We create the map with : file path -> List of differencse
      mapPathAndListDifferences(mergedMap.keys, differences, Map())
    }

  }


  /**
   *
   * @param listFilesDiffs
   * @param acc
   * @return
   */
  def prettyFormat(listFilesDiffs: Map[String, List[String]], acc: String): String = {
    if (listFilesDiffs.isEmpty) acc
    else {
      val currentFile = listFilesDiffs.head
      val fileName = currentFile._1
      val diffs = currentFile._2.map(diff => {
        if (diff.split(" ")(0) == "+") s"   ${Console.GREEN}" + diff + Console.RESET
        else s"   ${Console.RED}" + diff + Console.RESET
      })
      val format = fileName + "\n" + diffs.mkString("\n")
      prettyFormat(listFilesDiffs.tail, acc + format)
    }
  }


  /**
   *
   * @param map1 Map[String, String]
   * @param map2 Map[String, String]
   * @param acc Map[String, List[String]]
   * @return Map[String, List[String]]
   * Given 2 maps, returns the map1 and map2 merged (based on the keys of map1)
   * The map2 keys that aren't in the map1 keys will not be processed
   */
  @tailrec
  private def mergeMaps(map1: Map[String, String], map2: Map[String, String], acc: Map[String, List[String]]): Map[String, List[String]] = {
    if (map1.isEmpty) return acc
    else mergeMaps(map1.tail, map2, acc + (map1.head._1 -> List(map1.head._2, map2.getOrElse(map1.head._1, ""))))
  }


  /**
   *
   * @param listPaths Iterable[String]
   * @param listDifferences Iterable[ List[String] ]
   * @param acc Map[ String, List[String] ]
   * @return Map[ String, List[String] ]
   * Given an array of keys and an array of values, returns the map associated
   * Pre-conditions = listPaths.length == listDifferences.length
   */
  @tailrec
  private def mapPathAndListDifferences(listPaths: Iterable[String], listDifferences: Iterable[List[String]], acc: Map[String, List[String]]): Map[String, List[String]] = {
    if (listPaths.isEmpty) acc
    else mapPathAndListDifferences(listPaths.tail, listDifferences.tail, acc + (listPaths.head -> listDifferences.head))
  }


  /**
   *
   * @param listKeys Array[String]
   * @param listValues Array[String]
   * @param acc Map[String, String]
   * @return Map[String, String]
   * Given an array of keys and an array of values, returns the map associated
   * Pre-conditions = listKeys.length == listValues.length
   */
  @tailrec
  private def mapPathAndContent(listKeys: Array[String], listValues: Array[String], acc: Map[String, String]): Map[String, String] = {
    if (listKeys.isEmpty) acc
    else mapPathAndContent(listKeys.tail, listValues.tail, acc + (listKeys.head -> listValues.head))
  }


  /**
   *
   * @param rootPath String
   * @param stageContent String
   * @param filesInCurrentDirectory List[String]
   * @param listHashAndFilesDirectory List[List[String]]
   * @return an array of string
   * Returns modified files ie files present in the stage file and in the directory but having different hash
   */
  private def getModifiedFiles(rootPath: String, stageContent: String, filesInCurrentDirectory: List[String], listHashAndFilesDirectory: List[List[String]]): Array[String] = {
    val stageContentTab = stageContent.split("\n")

    if (stageContent == "") Array()
    else {
      val listHashAndFilesStage = stageContentTab.map(elem => List(elem.split(" ")(1), elem.split(" ")(2)))
      listHashAndFilesStage.diff(listHashAndFilesDirectory).map(elem => elem(1)).distinct
    }
  }


  /**
   *
   * @param text1 List[String]
   * @param text2 List[String]
   * @param index1 Int
   * @param index2 Int
   * @param matrix Map[(Int,Int), Int]
   * @param acc List[String]
   * @return List[String]
   * Given 2 lists which represents 2 texts, returns the list of deletions and additions of the text2 comparing to the text1
   * Pre-conditions : index1 = text1.length-1, index2 = text2.length-1, acc = List()
   * The matrix parameter corresponds to the result of mostLargestCommonSubSetMatrix
   */
  @tailrec
  def getDifferences(text1: List[String], text2: List[String], index1: Int, index2: Int, matrix: Map[(Int, Int), Int], acc: List[String]): List[String] = {
    val currentElem = matrix.getOrElse((index1, index2), -1)
    val previousElemLine = matrix.getOrElse((index1, index2 - 1), 0)
    val previousElemCol = matrix.getOrElse((index1 - 1, index2), 0)

    if (currentElem == -1) {
      if (index1 == -1 & index2 == -1) acc
      else if (index1 == -1) text2.dropRight(text2.length - 1 - index2).map(elem => "+ " + elem) ++ acc
      else text1.dropRight(text1.length - 1 - index1).map(elem => "- " + elem) ++ acc
    }

    else {
      if (currentElem == previousElemLine) getDifferences(text1, text2, index1, index2 - 1, matrix, "+ " + text2(index2) :: acc)
      else if (currentElem == previousElemCol) getDifferences(text1, text2, index1 - 1, index2, matrix, "- " + text1(index1) :: acc)
      else getDifferences(text1, text2, index1 - 1, index2 - 1, matrix, acc)
    }
  }


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
  @tailrec
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
        else mostLargestCommonSubSetMatrix(list1, list2, newIndex1, newIndex2 + 1, acc + ((newIndex1, newIndex2) -> acc.getOrElse((newIndex1, newIndex2 - 1), 0)))
      }

      //If we are in the first column
      else if (newIndex2 == 0) {
        if (list1(newIndex1) == list2(newIndex2)) mostLargestCommonSubSetMatrix(list1, list2, newIndex1, newIndex2 + 1, acc + ((newIndex1, newIndex2) -> 1))
        else if (newIndex1 == 0) mostLargestCommonSubSetMatrix(list1, list2, newIndex1, newIndex2 + 1, acc + ((newIndex1, newIndex2) -> 0))
        else mostLargestCommonSubSetMatrix(list1, list2, newIndex1, newIndex2 + 1, acc + ((newIndex1, newIndex2) -> acc.getOrElse((newIndex1 - 1, newIndex2), 0)))
      }

      else { //Neither the first column nor the first line
        if (list1(newIndex1) == list2(newIndex2)) {
          val newValue = acc.getOrElse((newIndex1 - 1, newIndex2 - 1), 0) + 1
          mostLargestCommonSubSetMatrix(list1, list2, newIndex1, newIndex2 + 1, acc + ((newIndex1, newIndex2) -> newValue))
        }
        else {
          val maxValue = Math.max(acc.getOrElse((newIndex1, newIndex2 - 1), 0), acc.getOrElse((newIndex1 - 1, newIndex2), 0))
          mostLargestCommonSubSetMatrix(list1, list2, newIndex1, newIndex2 + 1, acc + ((newIndex1, newIndex2) -> maxValue))
        }
      }
    }
  }

}

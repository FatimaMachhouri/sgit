package command

import java.io.File
import entities.Tree
import scala.io.Source
import scala.util.matching.Regex

object Commit {

  /**
   *
   * Creates the arborescence of the stage file
   */
  def commit(): Unit = {
    val arrayPaths = formatStageFile()

    def commitTailRec(parentTree: String, treeName: String, arrayPaths: Array[Array[String]]): List[Tree] = {
      if (arrayPaths.length==0) return Nil
      else {
        if (isATxtFile(arrayPaths.head.head)) {
          createTree(parentTree, treeName, Array(arrayPaths.head.head)) :: commitTailRec(parentTree, treeName, arrayPaths.tail)
        }
        else { //I'm a directory
          val newArrayPaths = arrayPaths
          createTree(parentTree, arrayPaths.head.head, Array(arrayPaths.head.tail(0))) :: commitTailRec(arrayPaths.head.head, arrayPaths.head.tail.head, newArrayPaths)
        }
      }

    }
    val listTrees = commitTailRec("origin", "o", arrayPaths)
  }


  /**
   *
   * @param parentTree
   * @param treeName
   * @param components
   * @return a Tree with the corresponding parameters
   */
  private def createTree(parentTree: String, treeName: String, components: Array[String]): Tree = {
    val trees = components.filter(!isATxtFile(_))
    val blobs = components.filter(isATxtFile(_))
    Tree(parentTree, treeName, trees, blobs)
  }


  /**
   *
   * @param fileName
   * @return True if the file is a txt else false
   */
  private def isATxtFile(fileName: String): Boolean = {
    val numPattern = new Regex("^([a-zA-Z0-9\\s_\\\\.\\-\\(\\):])+\\.(txt)$")
    numPattern.findAllIn(fileName).nonEmpty
  }


  /**
   *
   * @return
   *
   * Each array corresponds to a path and in each array we have the path which is splitted in an array
   * For exemple : a1/b1/file1 and a2/b2/file2 --> [ [a1, b1, file1], [a2, b2, file2] ]
   */
  private def formatStageFile(): Array[Array[String]] = {
    val currentRepositoryPath = new File(".").getCanonicalPath + File.separator + "s.git" + File.separator + "STAGE"

    //We retrive the STAGE file content
    val fileContent = Source.fromFile(currentRepositoryPath).mkString

    //We split to have an array in which each table box corresponds to a line of the STAGE file
    val fileContentTab = fileContent.split("\n")

    //We keep only the path and we delete the blod id
    val fileContentTabWithoutBlob = fileContentTab.map(line => line.split(" ")(1))

    //Each path (array) is splitted into an array of its components (files and directories)
    fileContentTabWithoutBlob.map(path => path.split("/"))
  }

}

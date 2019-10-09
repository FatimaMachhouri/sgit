package command

import java.io.File
import entities.Tree
import scala.annotation.tailrec
import scala.io.Source

object Commit {

  /**
   *
   * @return
   */
  def commit(): List[String] = {
    //We get the stage content
    val stage = new File(".").getCanonicalPath + File.separator + ".sgit" + File.separator + "STAGE"
    val stageContent = Source.fromFile(stage).mkString

    //We split in order to have each line of the stage in a box. A line has the form : Blob hash path
    val currentStage = stageContent.split("\n").toList

    @tailrec
    def commitTailRec(currentStage: List[String]): List[String] = {
      if (areAllOriginChildren(currentStage)) {
        currentStage
      }
      else {
        //Step 1 :
        val deepest = deepestTrees(currentStage)

        //Step 2 :
        val deepestTreesMerged = merge(deepest)

        //Step 3 :
        val create = createTrees(deepestTreesMerged)

        //Step 4 :
        //TODO
        List()
      }
    }

    commitTailRec(currentStage)
  }


  private def deepestTrees(listPaths: List[String]): List[String] = {
    //We get only the path
    val tmpListPaths = listPaths.map(path => path.split(" ")(2))

    //We retrieve the length of the deepest tree(s)
    @tailrec
    def deepestTreesLengthTailRec(listPaths: List[String], maxLength: Int): Int = {
      if (listPaths.isEmpty) maxLength
      else {
        val currentPath = listPaths.head.split("/")
        if (currentPath.length > maxLength) deepestTreesLengthTailRec(listPaths.tail, currentPath.length)
        else deepestTreesLengthTailRec(listPaths.tail, maxLength)
      }
    }

    val maxLength = deepestTreesLengthTailRec(tmpListPaths, 0)
    listPaths.filter(path => {path.split(" ")(2).split("/").length == maxLength})
  }


  private def merge(listPaths: List[String]): List[Tree] = {

    @tailrec
    def mergeTailRec(listPaths: List[String], acc: List[Tree]): List[Tree] = {
      if (listPaths.isEmpty) acc
      else {
        val currentPath = listPaths.head
        val currentPathTab = currentPath.split(" ")(2).split("/")
        val pathTree = currentPathTab.dropRight(1).mkString("/")
        val contentTree = currentPath.split(" ")(0) + " " + currentPath.split(" ")(1) + " " + currentPathTab(currentPathTab.length-1)

        val treeWithSamePath = acc.filter(elem => elem.treePath == pathTree)

        if (treeWithSamePath.isEmpty) mergeTailRec(listPaths.tail, Tree(pathTree, List(contentTree)) :: acc)
        else {
          val oldContentTree = acc.filter(elem => elem.treePath == pathTree).head.content
          val newContentTree = contentTree :: oldContentTree
          val newAcc = acc.filter(elem => elem.treePath != pathTree)

          mergeTailRec(listPaths.tail, Tree(pathTree, newContentTree) :: newAcc)
        }
      }
    }

    mergeTailRec(listPaths, List())
  }


  private def createTrees(listPaths: List[Tree]): List[String] = {
    List()
  }


  private def areAllOriginChildren(listPaths: List[String]): Boolean = {
    val tmpListPaths = listPaths.map(path => path.split(" ")(2))
    val elementsWithSubElements = tmpListPaths.filter(path => path.split("/").length > 1)
    elementsWithSubElements.isEmpty
  }

}

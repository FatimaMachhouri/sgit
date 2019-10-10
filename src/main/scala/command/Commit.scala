package command

import java.io.File

import entities.Tree

import scala.annotation.tailrec
import scala.io.Source
import utils.FileIO.createTrees
import utils.FileIO.createRootTree
import utils.FileIO.createCommit
import utils.FileIO.updateBranch


object Commit {

  /**
   *
   */
  def commit(): Unit = {

    //Step 1 : We create all trees excepting the root tree
    val rootTree = createSubTreesofRoot()

    //Step 2 : We create the root tree
    val idRootTree = createRootTree(rootTree)

    val idRootTreeLastCommit = {
      //Step 1 : We get the branch
      val currentRepositoryPath = new File(".").getCanonicalPath
      val headPath = currentRepositoryPath + File.separator + ".sgit" + File.separator + "HEAD"
      val currentBranch = Source.fromFile(headPath).mkString

      //Step 2 :
      val branchPath = currentRepositoryPath + File.separator + ".sgit" + File.separator + "Branches" + File.separator + currentBranch
      val lastCommit = Source.fromFile(branchPath).mkString

      //Step 3 :
      val commitPath = currentRepositoryPath + File.separator + ".sgit" + File.separator + "Commits" + File.separator + lastCommit
      Source.fromFile(commitPath).mkString.split("\n")(1)
    }

    //We check if last tree of the commit isn't the same that the new one that we want to create. If same, we don't commit
    if(idRootTree != idRootTreeLastCommit) {
      //Step 3 : We create the commit object
      val idCommit = createCommit(idRootTree)

      //Step 4 : We update the branch object
      updateBranch(idCommit)

      //Step 5 : We update the log object
    }

  }

  /**
   *
   * @return
   */
  private def createSubTreesofRoot(): List[String] = {
    //We get the stage content
    val stage = new File(".").getCanonicalPath + File.separator + ".sgit" + File.separator + "STAGE"
    val stageContent = Source.fromFile(stage).mkString

    //We split in order to have each line of the stage in a box. A line has the form : Blob hash path
    val currentStage = stageContent.split("\n").toList

    @tailrec
    def commitTailRec(currentStage: List[String]): List[String] = {
      //Step 0 : We accumulate all origin children ie paths with the following format => "folder" or "file.txt"
      val accTab = currentStage.map(path => path.split(" "))
      val tmpAcc = accTab.filter(path => path(2).split(File.separator).length == 1)
      val acc = tmpAcc.map(elem => elem.mkString(" ") + " ")

      if (areAllOriginChildren(currentStage)) {
        acc
      }
      else {
        val currentStageTmp = currentStage.diff(acc)

        //Step 1 :
        val deepest = deepestTrees(currentStageTmp)

        //Step 2 :
        val deepestTreesMerged = merge(deepest.distinct)

        //Step 3 :
        val createdTrees = createTrees(deepestTreesMerged)

        //Step 4 :
        val newStage1 = currentStageTmp.diff(deepest)
        val newStage = newStage1 ++ createdTrees

        commitTailRec(newStage)
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
        val currentPath = listPaths.head.split(File.separator)
        if (currentPath.length > maxLength) deepestTreesLengthTailRec(listPaths.tail, currentPath.length)
        else deepestTreesLengthTailRec(listPaths.tail, maxLength)
      }
    }

    val maxLength = deepestTreesLengthTailRec(tmpListPaths, 0)
    val deepTrees = listPaths.filter(path => {path.split(" ")(2).split(File.separator).length == maxLength})

    deepTrees
  }


  private def merge(listPaths: List[String]): List[Tree] = {

    @tailrec
    def mergeTailRec(listPaths: List[String], acc: List[Tree]): List[Tree] = {
      if (listPaths.isEmpty) acc
      else {
        val currentPath = listPaths.head
        val currentPathTab = currentPath.split(" ")(2).split(File.separator)
        val pathTree = currentPathTab.dropRight(1).mkString(File.separator)
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


  private def areAllOriginChildren(listPaths: List[String]): Boolean = {
    val tmpListPaths = listPaths.map(path => path.split(" "))
    val elementsWithSubElements = tmpListPaths.filter(path => path(2).split(File.separator).length > 1)
    elementsWithSubElements.isEmpty
  }

}

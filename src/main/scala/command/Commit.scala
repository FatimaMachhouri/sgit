package command

import java.io.File
import java.nio.file.{Files, Paths}
import entities.Tree
import scala.annotation.tailrec
import utils.FileIO.{createCommit, createRootTree, createTrees, updateBranch, updateLogFile, getContentFile, writeInFile}

object Commit {

  /**
   *
   * @param rootPath : String
   */
  def commit(rootPath: String): Unit = {

    //Step 1 : We create all trees excepting the root tree
    val rootTree = createSubTreesofRoot(rootPath)

    //Step 2 : We create the root tree
    val idRootTree = createRootTree(rootPath, rootTree)

    //We get the last commit in order to not commit twice the same content consecutively
    val idRootTreeLastCommit = {
      //Step 1 : We get the branch name
      val headPath = rootPath + File.separator + ".sgit" + File.separator + "HEAD"
      val currentBranch = getContentFile(headPath)

      //Step 2 : if it is the initial commit, we don't have a branch file created. We check if the branch file exists or not
      val branchPath = rootPath + File.separator + ".sgit" + File.separator + "Branches" + File.separator + currentBranch
      val existsBranch = Files.exists(Paths.get(branchPath))

      //Step 3 : Based on the currentBranch, we get the last commit. If it's the initial commit, we assume that the lastCommit is 0000000000000000000000000000000000000000
      val lastCommit = {
        if (!existsBranch) "0000000000000000000000000000000000000000"
        else getContentFile(branchPath)
      }

      if (lastCommit == "0000000000000000000000000000000000000000") "InitialCommit"
      else {
        val commitPath = rootPath + File.separator + ".sgit" + File.separator + "Commits" + File.separator + lastCommit
        getContentFile(commitPath).split("\n")(1)
      }
    }

    //We check if last tree of the commit isn't the same that the new one that we want to create. If same, we don't commit
    if(idRootTree != idRootTreeLastCommit) {
      //We create the commit object
      val idCommit = createCommit(rootPath, idRootTree)

      //We update the branch object by adding the new commit
      updateBranch(rootPath, idCommit)

      //We update the log object by adding the new commit
      updateLogFile(rootPath, idCommit)

      //We update the stage file commit by copying the content of the stage
      writeInFile(rootPath + File.separator + ".sgit" + File.separator + "STAGECOMMIT", getContentFile(rootPath + File.separator + ".sgit" + File.separator + "STAGE"))
    }

  }


  /**
   *
   * @return a list of String
   * Creates the arborescence (except root tree) of the stage file.
   */
  private def createSubTreesofRoot(rootPath: String): List[String] = {
    //We get the stage content
    val stage = rootPath + File.separator + ".sgit" + File.separator + "STAGE"
    val stageContent = getContentFile(stage)

    //We split in order to have each line of the stage file in a box. A line of the stage file has the form : Blob Hash Path
    val currentStage = stageContent.split("\n").toList

    @tailrec
    def commitTailRec(currentStage: List[String]): List[String] = {
      //We accumulate all root children ie paths with the following format => "folderName" or "fileName.txt"
      val accTab = currentStage.map(path => path.split(" "))
      val tmpAcc = accTab.filter(path => path(2).split(File.separator).length == 1)
      val acc = tmpAcc.map(elem => elem.mkString(" ") + " ")

      if (areAllRootChildren(currentStage)) {
        acc
      }
      else {
        val currentStageTmp = currentStage.diff(acc)

        val deepest = deepestPaths(currentStageTmp)

        val deepestTreesMerged = merge(deepest.distinct)

        val createdTrees = createTrees(rootPath, deepestTreesMerged)

        val newStageTmp = currentStageTmp.diff(deepest)
        val newStage = newStageTmp ++ createdTrees

        commitTailRec(newStage)
      }
    }

    commitTailRec(currentStage)
  }


  /**
   *
   * @param listPaths
   * @return a list of string
   * Return the deespest paths of the list paramater ie those whose path is the longest
   */
  private def deepestPaths(listPaths: List[String]): List[String] = {
    //We get only the path
    val tmpListPaths = listPaths.map(path => path.split(" ")(2))

    //We retrieve the length of the deepest path(s)
    @tailrec
    def deepestPathsLengthTailRec(listPaths: List[String], maxLength: Int): Int = {
      if (listPaths.isEmpty) maxLength
      else {
        val currentPath = listPaths.head.split(File.separator)
        if (currentPath.length > maxLength) deepestPathsLengthTailRec(listPaths.tail, currentPath.length)
        else deepestPathsLengthTailRec(listPaths.tail, maxLength)
      }
    }

    val maxLength = deepestPathsLengthTailRec(tmpListPaths, 0)
    val deepPaths = listPaths.filter(path => path.split(" ")(2).split(File.separator).length == maxLength)

    deepPaths
  }


  /**
   *
   * @param listPaths
   * @return a list of trees
   * Takes in parameter path of the same length and merges path with the same root arborescence
   * merge(List("directory1/directory2/file1.txt", "directory1/directory2/file2.txt")) = Tree("directory1/directory2", List("file1.txt, file2.txt"))
   */
  private def merge(listPaths: List[String]): List[Tree] = {

    @tailrec
    def mergeTailRec(listPaths: List[String], acc: List[Tree]): List[Tree] = {
      if (listPaths.isEmpty) acc
      else {
        val currentPath = listPaths.head //format Type Hash Path
        val currentPathTab = currentPath.split(" ")(2).split(File.separator) //We retrieve only the path
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


  /**
   *
   * @param listPaths where an element of the list has the following format : "Type Hash Path"
   * @return a boolean
   * Returns true if all parameter paths are root children ie have the format : "x" or "y.txt" where x and y are respectively directory and file name
   */
  private def areAllRootChildren(listPaths: List[String]): Boolean = {
    val tmpListPaths = listPaths.map(path => path.split(" "))
    val elementsWithSubElements = tmpListPaths.filter(path => path(2).split(File.separator).length > 1)
    elementsWithSubElements.isEmpty
  }

}

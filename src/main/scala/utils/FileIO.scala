package utils

import java.io.{BufferedWriter, File, FileWriter}
import java.nio.file.{Files, Paths}
import java.text.SimpleDateFormat
import java.util.Calendar
import utils.Hash.encryptThisString
import entities.Tree
import scala.annotation.tailrec
import scala.io.Source


object FileIO {

  /**
   *
   * @param path String
   * @return Boolean
   * Creates in the path parameter a file. Returns true if the file is created otherwise false.
   */
  def createFile(path: String): Boolean = {
    val file = new File(path)
    file.createNewFile()
  }


  /**
   *
   * @param path String
   * @return Boolean
   * Creates in the path parameter a directory. Returns true if the directory is created otherwise false.
   */
  def createDirectory(path: String): Boolean = {
    val directory = new File(path)
    directory.mkdir()
  }


  /**
   *
   * @param path String
   * @param content String
   * Writes in the path parameter file the content parameter
   * Does nothing if the file doesn't exist
   */
  def writeInFile(path: String, content: String): Unit = {
    val file = new File(path)

    if (Files.exists(Paths.get(path))) {
      val bw = new BufferedWriter(new FileWriter(file))
      bw.write(content)
      bw.close()
    }
  }


  /**
   *
   * @param path String
   * @return String
   * Returns the content of the path file
   */
  def getContentFile(path: String): String = {
    Source.fromFile(path).mkString
  }


  /**
   *
   * @param listTrees List[Tree]
   * @return List[String]
   */
  def createTrees(rootPath: String, listTrees: List[Tree]): List[String] = {
    //We get the path
    val pathTrees = rootPath + File.separator + ".sgit" + File.separator + "Trees"

    listTrees.map(tree => {
      //We create the tree file based on the crypted content of the tree
      createFile(pathTrees + File.separator + tree.idTree())

      //We write in the file the content of the tree
      writeInFile(pathTrees + File.separator + tree.idTree(), tree.content.mkString("\n"))
    })

    //We return the list of new paths generated composed by the new trees and their hash
    listTrees.map(tree => "Tree " + tree.idTree() + " " + tree.treePath)
  }


  /**
   *
   * @param rootPath String
   * @param contentRoot List[String]
   * @return String
   * Creates the root tree based on the list parameter and return the hash string of the root tree
   */
  def createRootTree(rootPath: String, contentRoot: List[String]): String = {
    //We get the path
    val pathTrees = rootPath + File.separator + ".sgit" + File.separator + "Trees"

    val rootTree = Tree("root", contentRoot)

    createFile(pathTrees + File.separator + rootTree.idTree())
    writeInFile(pathTrees + File.separator + rootTree.idTree(), rootTree.content.mkString("\n"))

    rootTree.idTree()
  }


  /**
   *
   * @param rootPath String
   * @param commitTree String
   * @return String
   * Creates the commit file with its content (parent commit, tree, date). The name of the file is its content crypted
   * Returns the hash of the commit created.
   */
  def createCommit(rootPath: String, commitTree: String): String = {
    val pathCommits = rootPath + File.separator + ".sgit" + File.separator + "Commits"

    val formatDate = new SimpleDateFormat("y-M-d_hh:mm:ss::aa")

    val commitContent = getLastCommit(rootPath) + "\n" + commitTree + "\n" + formatDate.format(Calendar.getInstance().getTime())

    createFile(pathCommits + File.separator + encryptThisString(commitContent))
    writeInFile(pathCommits + File.separator + encryptThisString(commitContent), commitContent)

    encryptThisString(commitContent)
  }


  /**
   *
   * @param rootPath String
   * @return String
   * Returns the hash of the last commit
   */
  def getLastCommit(rootPath: String): String = {
    //We get the current branch
    val currentBranch = getContentFile(rootPath + File.separator + ".sgit" + File.separator + "HEAD")

    //We get the last commit in the branch file corresponding
    val pathBranches = rootPath + File.separator + ".sgit" + File.separator + "Branches"

    //If the file not exists, it's the initial commit
    if (!Files.exists(Paths.get(pathBranches + File.separator + currentBranch))) "None"
    else getContentFile(pathBranches + File.separator + currentBranch)
  }


  /**
   *
   * @param rootPath String
   * @return String
   * Creates the current branch file and returns its path
   */
  def createBranchFile(rootPath: String): String = {
    val currentBranch = getContentFile(rootPath + File.separator + ".sgit" + File.separator + "HEAD")

    createFile(rootPath + File.separator + ".sgit" + File.separator + "Branches" + File.separator + currentBranch)
    rootPath + File.separator + ".sgit" + File.separator + "Branches" + File.separator + currentBranch
  }


  /**
   *
   * @param rootPath String
   * @param commitId String
   * Write the commitId in the current branch file.
   * If the branch doesn't exists, it creates it and write the last commit id. Else, it replaces the last commit id.
   */
  def updateBranch(rootPath: String, commitId: String): Unit = {
    val parentCommit = getLastCommit(rootPath)
    val pathBranchFile = {
      if (parentCommit == "None") createBranchFile(rootPath)
      else {
        val headPath = rootPath + File.separator + ".sgit" + File.separator + "HEAD"
        val currentBranch = getContentFile(headPath)
        rootPath + File.separator + ".sgit" + File.separator + "Branches" + File.separator + currentBranch
      }
    }
    writeInFile(pathBranchFile, commitId)
  }


  /**
   *
   * @param rootPath String
   * @param idCommit
   * Writes in the log file of the current branch the current commit and its parent commit
   */
  def updateLogFile(rootPath: String, idCommit: String): Unit = {
    val headPath = rootPath + File.separator + ".sgit" + File.separator + "HEAD"
    val currentBranch = getContentFile(headPath)

    val logPath = rootPath + File.separator + ".sgit" + File.separator + "Logs" + File.separator + currentBranch

    val commit = rootPath + File.separator + ".sgit" + File.separator + "Commits" + File.separator + idCommit
    val previousCommit = getContentFile(commit).mkString.split("\n")(0)
    val formatDate = new SimpleDateFormat("y-M-d_hh:mm:ss::aa")

    val logFileContent = previousCommit + " " + idCommit + " " + formatDate.format(Calendar.getInstance().getTime())

    val bw = new BufferedWriter(new FileWriter(logPath, true))
    bw.write(logFileContent + "\n")
    bw.close()
  }


  /**
   *
   * @param rootPath String
   * @return String
   */
  def getCurrentBranch(rootPath: String): String = {
    val headFilePath = rootPath + File.separator + ".sgit" + File.separator + "HEAD"
    getContentFile(headFilePath)
  }


  /**
   *
   * @param rootPath String
   * @return Boolean
   */
  def isADirectory(rootPath: String): Boolean = {
    new File(rootPath).isDirectory
  }


  /**
   *
   * @param rootPath String
   * @param commit String
   * @return List[String]
   * Returns the blobs present in the stage when the commit parameter was created
   * The commit corresponds to the hash of the commit we want to recreate the tree
   * A blob has the following format : Blob Hash Path
   */
  def recreateTree(rootPath: String, commit: String): List[String] = {
    val rootTreeHash = getContentFile(rootPath + File.separator + ".sgit" + File.separator + "Commits" + File.separator + commit).split("\n")(1)

    //The map key is the path of a hash tree and the value is the hash tree
    @tailrec
    def recreateTreeTailRec(treesToProcess: Map[String, String], acc: List[String]): List[String] = {
      if (treesToProcess.isEmpty) acc

      else if (!getContentFile(rootPath + File.separator + ".sgit" + File.separator + "Trees" + File.separator + treesToProcess.head._2).contains("Tree")) { //if we have only blobs in the currentTree
        val currentTreeHash = treesToProcess.head._2
        val blobs = getContentFile(rootPath + File.separator + ".sgit" + File.separator + "Trees" + File.separator + currentTreeHash).split("\n")
        recreateTreeTailRec(treesToProcess.tail, acc ++ blobs.map(elem => "Blob " + elem.split(" ")(1) + " " + treesToProcess.head._1 + File.separator + elem.split(" ")(2)))
      }

      else { //We have at least 1 tree in the current tree
        val currentTreeHash = treesToProcess.head._2
        val treeContent = getContentFile(rootPath + File.separator + ".sgit" + File.separator + "Trees" + File.separator + currentTreeHash).split("\n")
        val blobs = treeContent.filter(elem => elem.split(" ")(0) == "Blob")
        val subTrees = treeContent.filter(elem => elem.split(" ")(0) == "Tree")
        val subTreesHash = subTrees.map(elem => (treesToProcess.head._1 + File.separator + elem.split(" ")(2) -> elem.split(" ")(1))) //we get pathTree -> hashTree

        recreateTreeTailRec(treesToProcess.tail ++ subTreesHash, acc ++ blobs.map(elem => "Blob " + elem.split(" ")(1) + " " + treesToProcess.head._1 + File.separator + elem.split(" ")(2)))
      }
    }
    recreateTreeTailRec(Map(("" -> rootTreeHash)), List()).map(elem => "Blob " + elem.split(" ")(1) + " " + elem.split(" ")(2).drop(1)) //we drop the first file separator /test/test.txt -> test/test.txt
  }

}

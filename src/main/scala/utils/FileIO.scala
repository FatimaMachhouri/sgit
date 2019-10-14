package utils

import java.io.{BufferedWriter, File, FileWriter}
import java.nio.file.{Files, Paths}
import java.text.SimpleDateFormat
import java.util.Calendar
import utils.Hash.encryptThisString
import entities.Tree
import scala.io.Source


object FileIO {

  /**
   *
   * @param path string
   * @return a boolean
   * Creates in the path parameter a file. Returns true if the file is created otherwise false.
   */
  def createFile(path: String): Boolean = {
    val file = new File(path)
    file.createNewFile()
  }


  /**
   *
   * @param path string
   * @return a boolean
   * Creates in the path parameter a directory. Returns true if the directory is created otherwise false.
   */
  def createDirectory(path: String): Boolean = {
    val directory = new File(path)
    directory.mkdir()
  }


  /**
   *
   * @param path
   * @param content
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
   * @param path
   * @return a string
   * Returns the content of the path file
   */
  def getContentFile(path: String): String = {
    Source.fromFile(path).mkString
  }


  /**
   *
   * @param listTrees
   * @return a list of string
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
   * @param contentRoot a list of string
   * @return a string
   * Creates the root tree based on the list parameter and return the hash string of the root tree
   */
  def createRootTree(rootPath: String, contentRoot: List[String]): String = {
    //We get the path
    val pathTrees = rootPath + File.separator + ".sgit" + File.separator + "Trees"

    val rootTree = Tree("root", contentRoot)

    println("PATH " + pathTrees + File.separator + rootTree.idTree())

    createFile(pathTrees + File.separator + rootTree.idTree())
    writeInFile(pathTrees + File.separator + rootTree.idTree(), rootTree.content.mkString("\n"))

    rootTree.idTree()
  }


  /**
   *
   * @param commitTree
   * @return a string
   * Creates the commit file with its content (parent commit, tree, date). The name of the file is its content crypted
   * Returns the hash of the commit created.
   */
  def createCommit(rootPath: String, commitTree: String): String = {
    val pathCommits = rootPath + File.separator + ".sgit" + File.separator + "Commits"

    val formatDate = new SimpleDateFormat("d-M-y hh:mm:ss aa")

    val commitContent = getLastCommit(rootPath) + "\n" + commitTree + "\n" + formatDate.format(Calendar.getInstance().getTime())

    createFile(pathCommits + File.separator + encryptThisString(commitContent))
    writeInFile(pathCommits + File.separator + encryptThisString(commitContent), commitContent)

    encryptThisString(commitContent)
  }


  /**
   *
   * @return a string
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
   * @return a string
   * Creates the current branch file and returns its path
   */
  def createBranchFile(rootPath: String): String = {
    val currentBranch = getContentFile(rootPath + File.separator + ".sgit" + File.separator + "HEAD")

    createFile(rootPath + File.separator + ".sgit" + File.separator + "Branches" + File.separator + currentBranch)
    rootPath + File.separator + ".sgit" + File.separator + "Branches" + File.separator + currentBranch
  }


  /**
   *
   * @param commitId
   * Write the commitId in the current branch file.
   * If the branch doesn't exists, it creates it and write the last commit id. Else, it replaces the last commit id.
   */
  def updateBranch(rootPath: String, commitId: String): Unit = {
    val parentCommit = getLastCommit(rootPath)
    val pathBranchFile = {
      if (parentCommit == "None") createBranchFile(rootPath)
      else {
        val headPath = new File(".").getCanonicalPath + File.separator + ".sgit" + File.separator + "HEAD"
        val currentBranch = getContentFile(headPath)
        new File(".").getCanonicalPath + File.separator + ".sgit" + File.separator + "Branches" + File.separator + currentBranch
      }
    }
    writeInFile(pathBranchFile, commitId)
  }


  /**
   *
   * @param idCommit
   * Writes in the log file of the current branch the current commit and its parent commit
   */
  def updateLogFile(rootPath: String, idCommit: String): Unit = {
    val headPath = rootPath + File.separator + ".sgit" + File.separator + "HEAD"
    val currentBranch = getContentFile(headPath)

    val logPath = rootPath + File.separator + ".sgit" + File.separator + "Logs" + File.separator + currentBranch

    val commit = rootPath + File.separator + ".sgit" + File.separator + "Commits" + File.separator + idCommit
    val previousCommit = getContentFile(commit).mkString.split("\n")(0)

    val bw = new BufferedWriter(new FileWriter(logPath, true))
    bw.write(previousCommit + " " + idCommit + "\n")
    bw.close()
  }


  /**
   *
   * @param rootPath
   * @return a string
   */
  def getCurrentBranch(rootPath: String): String = {
    val headFilePath = rootPath + File.separator + ".sgit" + File.separator + "HEAD"
    getContentFile(headFilePath)
  }


  /**
   *
   * @param rootPath
   * @return
   */
  def isADirectory(rootPath: String): Boolean = {
    new File(rootPath).isDirectory
  }

}

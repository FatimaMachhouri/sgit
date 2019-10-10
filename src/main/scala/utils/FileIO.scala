package utils

import java.io.{BufferedWriter, File, FileWriter}
import java.nio.file.{Files, Paths}
import java.text.SimpleDateFormat
import java.util.Calendar

import utils.Hash.encryptThisString
import entities.Tree

import scala.io.Source

object FileIO {
  def createTrees(listTrees: List[Tree]): List[String] = {
    //We get the path
    val currentRepositoryPath = new File(".").getCanonicalPath
    val pathTrees = currentRepositoryPath + File.separator + ".sgit" + File.separator + "Trees"

    listTrees.map(tree => {
      //We create the tree file based on the crypted content of the tree
      val treeFile = new File(pathTrees + File.separator + tree.idTree())
      treeFile.createNewFile()

      //We write in the file the content of the tree
      val bw = new BufferedWriter(new FileWriter(pathTrees + File.separator + tree.idTree()))
      bw.write(tree.content.mkString("\n"))
      bw.close()
    })

    listTrees.map(tree => {
      "Tree " + tree.idTree() + " " + tree.treePath
    })

  }

  def createRootTree(contentRoot: List[String]): String = {
    //We get the path
    val currentRepositoryPath = new File(".").getCanonicalPath
    val pathTrees = currentRepositoryPath + File.separator + ".sgit" + File.separator + "Trees"

    val rootTree = Tree("root", contentRoot)

    val rootTreeFile = new File(pathTrees + File.separator + rootTree.idTree())
    rootTreeFile.createNewFile()

    //We write in the file the content of the tree
    val bw = new BufferedWriter(new FileWriter(pathTrees + File.separator + rootTree.idTree()))
    bw.write(rootTree.content.mkString("\n"))
    bw.close()

    rootTree.idTree()
  }

  def createCommit(commitTree: String): String = {
    //We get the path
    val currentRepositoryPath = new File(".").getCanonicalPath
    val pathCommits = currentRepositoryPath + File.separator + ".sgit" + File.separator + "Commits"

    val formatDate = new SimpleDateFormat("d-M-y hh:mm:ss aa")

    val commitContent = getLastCommit() + "\n" + commitTree + "\n" + formatDate.format(Calendar.getInstance().getTime())

    val commitFile = new File(pathCommits + File.separator + encryptThisString(commitContent))
    commitFile.createNewFile()

    //We write in the file the content of the tree
    val bw = new BufferedWriter(new FileWriter(pathCommits + File.separator + encryptThisString(commitContent)))
    bw.write(commitContent)
    bw.close()

    encryptThisString(commitContent)
  }

  def getLastCommit(): String = {
    //We get the path
    val currentRepositoryPath = new File(".").getCanonicalPath

    //We get the current branch
    val headPath = currentRepositoryPath + File.separator + ".sgit" + File.separator + "HEAD"
    val currentBranch = Source.fromFile(headPath).mkString

    //We get the last commit in the branch file corresponding
    val pathBranches = currentRepositoryPath + File.separator + ".sgit" + File.separator + "Branches"
    val branchFileExists = Files.exists(Paths.get(pathBranches + File.separator + currentBranch))
    //If the file ! exists, it's the initial commit
    if (!branchFileExists) {
      "None"
    }
    else {
      //We get the last commit
      Source.fromFile(pathBranches + File.separator + currentBranch).mkString
    }
  }

  def createBranchFile(): String = {
    val currentRepositoryPath = new File(".").getCanonicalPath

    //We get the current branch
    val headPath = currentRepositoryPath + File.separator + ".sgit" + File.separator + "HEAD"
    val currentBranch = Source.fromFile(headPath).mkString

    new File(currentRepositoryPath + File.separator + ".sgit" + File.separator + "Branches" + File.separator + currentBranch).createNewFile()

    currentRepositoryPath + File.separator + ".sgit" + File.separator + "Branches" + File.separator + currentBranch
  }

  def updateBranch(commit: String): Unit = {
    //If the branch doesn't exists, we create it and write the last commit id
    //Else, we replace the last commit id
    val parentCommit = getLastCommit()
    val pathBranchFile = {
      if (parentCommit == "None") createBranchFile()
      else {
        val headPath = new File(".").getCanonicalPath + File.separator + ".sgit" + File.separator + "HEAD"
        val currentBranch = Source.fromFile(headPath).mkString

        new File(".").getCanonicalPath + File.separator + ".sgit" + File.separator + "Branches" + File.separator + currentBranch
      }
    }
    val bws = new BufferedWriter(new FileWriter(pathBranchFile))
    bws.write(commit)
    bws.close()
  }

  def updateLogFile(idCommit: String): Unit = {
    //We get the current branch
    val headPath = new File(".").getCanonicalPath + File.separator + ".sgit" + File.separator + "HEAD"
    val currentBranch = Source.fromFile(headPath).mkString

    val logPath = new File(".").getCanonicalPath + File.separator + ".sgit" + File.separator + "Logs" + File.separator + currentBranch
    val logFileExists = Files.exists(Paths.get(logPath))

    val commit = new File(".").getCanonicalPath + File.separator + ".sgit" + File.separator + "Commits" + File.separator + idCommit
    val previousCommit = Source.fromFile(commit).mkString.split("\n")(0)

    val bw = new BufferedWriter(new FileWriter(logPath, true))
    bw.write(previousCommit + " " + idCommit + "\n")
    bw.close()
  }

}

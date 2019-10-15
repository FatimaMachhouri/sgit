package command

import java.io.File

import utils.FileIO.getContentFile
import utils.Path.getFilesDirectory
import utils.FileIO.recreateTree
import scala.annotation.tailrec
import command.Diff.{getDifferences, mostLargestCommonSubSetMatrix}

object Log {

  /**
   *
   * @param rootPath String
   * @return Map[String, List[String]]
   * Returns for each branch, the lists of commits in the format commitHash Date
   * For each branch, commits are sorted from the most recent to the oldest
   * For the sgit log
   */
  def listCommits(rootPath: String): Map[String, List[String]] = {
    val listBranches = getFilesDirectory(rootPath + File.separator + ".sgit" + File.separator + "Logs")

    @tailrec
    def listCommitsTailRec(listBranches: List[String], acc: List[String]): List[String] = {
      if (listBranches.isEmpty) acc
      else {
        val currentBranchCommits = getContentFile(listBranches.head).split("\n")
        val commitsBranchFormat = currentBranchCommits.map(elem => listBranches.head.replace(rootPath + File.separator + ".sgit" + File.separator + "Logs" + File.separator, "") + " " + elem.split(" ")(1) + " " + elem.split(" ")(2))
        listCommitsTailRec(listBranches.tail, acc ++ commitsBranchFormat)
      }
    }

    listCommitsTailRec(listBranches, List()).groupBy(_.split(" ")(0)).map(elem => (elem._1, elem._2.reverse))
  }


  /**
   *
   * @param rootPath String
   * @param commit1 String
   * @param commit2 String
   * @return Map[String, List[String]]
   * Returns a map which contains for each file, the differences between 2 commits : the key is the file path and the value is the list of differences
   * If no differences, file -> List()
   */
  def diffCommits(rootPath: String, commit1: String, commit2: String): Map[String, List[String]] = {
    val commitFilesContent1 = recreateTree(rootPath, commit1).map(file => (file.split(" ")(2), getContentFile(rootPath + File.separator + ".sgit" + File.separator + "Blobs" + File.separator + file.split(" ")(1)))).toMap
    val commitFilesContent2 = recreateTree(rootPath, commit2).map(file => (file.split(" ")(2), getContentFile(rootPath + File.separator + ".sgit" + File.separator + "Blobs" + File.separator + file.split(" ")(1)))).toMap

    //Those who are in commitFilesContent2 but not in commitFilesContent1
    val newFiles = commitFilesContent2.filter(elem => !commitFilesContent1.keys.toList.contains(elem._1))

    val diffFiles = commitFilesContent2.filter(elem => !newFiles.keys.toList.contains(elem._1))

    diffFiles.map(elem => {
      val file = elem._1
      val content1 = commitFilesContent1.getOrElse(file, "").split("\n").toList
      val content2 = elem._2.split("\n").toList
      val matrix = mostLargestCommonSubSetMatrix(content1, content2, 0, 0, Map())
      val listDifferences = getDifferences(content1, content2, content1.length-1, content2.length-1, matrix, List())

      (file, listDifferences)
    })
  }


  /**
   *
   * @param rootPath String
   * @param commit1 String
   * @param commit2 String
   * @return List[String]
   * Given 2 commits, returns the new files created (present in the commit2 but not in the commit1)
   */
  def newFiles(rootPath: String, commit1: String, commit2: String): List[String] = {
    val commitFiles1 = recreateTree(rootPath, commit1).map(file => (file.split(" ")(2)))
    val commitFiles2 = recreateTree(rootPath, commit2).map(file => (file.split(" ")(2)))

    commitFiles2.diff(commitFiles1)
  }
  
}

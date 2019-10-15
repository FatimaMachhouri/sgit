package command

import command.Diff.{getDifferences, mostLargestCommonSubSetMatrix}
import utils.FileIO.{getContentFile,recreateTree}
import entities.Commit
import utils.Path.getFilesDirectory
import scala.annotation.tailrec
import java.io.File


object Log {

  /**
   *
   * @param rootPath String
   * @return Map[String, List[Commit]]
   * Returns for each branch, the lists of commits
   * A commit contains its hash, its parent, its date, its list of differences and its new files (comparing to its parent)
   * For each branch, commits are sorted from the most recent to the oldest
   */
  def listCommits(rootPath: String): Map[String, List[Commit]] = {
    val listBranches = getFilesDirectory(rootPath + File.separator + ".sgit" + File.separator + "Logs")

    @tailrec
    def listCommitsTailRec(listBranches: List[String], acc: Map[String, List[Commit]]): Map[String, List[Commit]] = {
      if (listBranches.isEmpty) acc
      else {
        val currentBranchCommits = getContentFile(listBranches.head).split("\n")
        val commitsBranch = currentBranchCommits.map(elem => {
          val commitHash = elem.split(" ")(1)
          val commitParentHash = elem.split(" ")(0)
          val date = elem.split(" ")(2)
          val newFilesCommit = newFiles(rootPath, commitParentHash, commitHash)
          val differences = diffCommits(rootPath, commitParentHash, commitHash)
          Commit(commitHash, commitParentHash, date, differences, newFilesCommit)
        })
        val branch = listBranches.head.replace(rootPath + File.separator + ".sgit" + File.separator + "Logs" + File.separator, "")
        listCommitsTailRec(listBranches.tail, acc + (branch -> commitsBranch.toList))
      }
    }

    listCommitsTailRec(listBranches, Map()).map(elem => (elem._1, elem._2.reverse))
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
  private def diffCommits(rootPath: String, commit1: String, commit2: String): Map[String, List[String]] = {
    if (commit1 == "None") { //Initial commit
      val commitFilesContent2 = recreateTree(rootPath, commit2).map(file => (file.split(" ")(2), getContentFile(rootPath + File.separator + ".sgit" + File.separator + "Blobs" + File.separator + file.split(" ")(1)))).toMap
      commitFilesContent2.map(elem => (elem._1, elem._2.split("\n").map(difference => "+ " + difference).toList))
    }

    else {
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
        val listDifferences = getDifferences(content1, content2, content1.length - 1, content2.length - 1, matrix, List())

        (file, listDifferences)
      })
    }

  }


  /**
   *
   * @param rootPath String
   * @param commit1 String
   * @param commit2 String
   * @return List[String]
   * Given 2 commits, returns the new files created (present in the commit2 but not in the commit1)
   */
  private def newFiles(rootPath: String, commit1: String, commit2: String): List[String] = {
    if (commit1 == "None") recreateTree(rootPath, commit2).map(file => file.split(" ")(2)) //Initial commit
    else {
      val commitFiles1 = recreateTree(rootPath, commit1).map(file => (file.split(" ")(2)))
      val commitFiles2 = recreateTree(rootPath, commit2).map(file => (file.split(" ")(2)))
      commitFiles2.diff(commitFiles1)
    }
  }

}

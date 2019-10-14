package command

import java.io.File

import utils.FileIO.getContentFile
import utils.Path.getFilesDirectory

import scala.annotation.tailrec

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


}

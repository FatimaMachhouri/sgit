package command

import command.Diff.{getDifferences, mostLargestCommonSubSetMatrix}
import utils.FileIO.{getContentFile, recreateTree}
import utils.Path.getFilesDirectory
import scala.annotation.tailrec
import java.io.File
import entities.Commit

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
   * @param logs Map[String, List[Commit]]
   * @return String
   */
  def prettyFormatLog(logs: Map[String, List[Commit]]): String = {

    @tailrec
    def prettyFormatLogTailRec(logs: Map[String, List[Commit]], acc: List[String]): String = {
      if (logs.isEmpty) acc.mkString("\n")
      else {
        val commits = logs.head._2.map(commit => s"${Console.YELLOW}  commit " + commit.commitHash + Console.RESET + " (" + logs.head._1 + ")" + "\n" + "  Date:  " + commit.date + "\n")
        prettyFormatLogTailRec(logs.tail, logs.head._1 + "\n" + commits.mkString("\n") :: acc)
      }
    }

    prettyFormatLogTailRec(logs, List())
  }


  /**
   *
   * @param logs Map[String, List[Commit]]
   * @return String
   */
  def prettyFormatLogP(logs: Map[String, List[Commit]]): String = {

    @tailrec
    def prettyFormatLogPTailRec(logs: Map[String, List[Commit]], acc: List[String]): String = {
      if (logs.isEmpty) acc.mkString("\n")
      else {
        val commits = logs.head._2.map(commit => {
          val newFiles = commit.listNewFiles.map(file => "   " + file).mkString("\n")

          val deletions = commit.listDifferences.toList.map(file => (file._1, file._2.filter(e => e.charAt(0) == '-'))).filter(elem => elem._2.nonEmpty) //for each file, we generate its deletions
          val additions = commit.listDifferences.toList.map(file => (file._1, file._2.filter(e => e.charAt(0) == '+'))).filter(elem => elem._2.nonEmpty) //same with additions

          val deletionsRed = deletions.map(file => (file._1, file._2.map(deletion => s"${Console.RED}" + deletion + Console.RESET)))
          val additionsGreen = additions.map(file => (file._1, file._2.map(addition => s"${Console.GREEN}" + addition + Console.RESET)))

          val listDiffs = (deletionsRed ++ additionsGreen).groupBy(file => file._1)

          val differences = listDiffs.toList.map(elem => (elem._1, elem._2.flatMap(e => e._2)))
          val differencesString = differences.map(file => "   " + file._1 + "\n" + file._2.map(elem => "     " + elem).mkString("\n")).mkString("\n")

            s"${Console.YELLOW}  commit " +
              commit.commitHash + Console.RESET +
              " ("+ logs.head._1 + ")" + "\n" +
              "  Date:  " + commit.date + "\n" +
              "  new files :\n" + newFiles + "\n" +
              "  diff :\n" + differencesString + "\n"

        })

        prettyFormatLogPTailRec(logs.tail, logs.head._1 + "\n" + commits.mkString("\n") :: acc)
      }
    }

    prettyFormatLogPTailRec(logs, List())
  }


  /**
   *
   * @param logs Map[String, List[Commit]]
   * @return String
   */
  def prettyFormatLogStat(logs: Map[String, List[Commit]]): Unit = {
    logs.toList.map(f = elem => {
      println(elem._1)

      def plusMinus(list: List[String]): String = {
        s"${Console.GREEN}" + list.filter(elem => elem.charAt(0) == '+').map(elem => elem.charAt(0)).mkString("") + Console.RESET + s"${Console.RED}" +list.filter(elem => elem.charAt(0) == '-').map(elem => elem.charAt(0)).mkString("") + Console.RESET
      }

      elem._2.map(commit => {
        val differentFiles = commit.listDifferences.toList.map(elem => "  " + elem._1 + " | " + elem._2.length + " " + plusMinus(elem._2))
        val newEmptyFiles = commit.listNewFiles.diff(commit.listDifferences.keys.toList)

        val nbFilesChanged = newEmptyFiles.length + differentFiles.length
        val nbInsertions = differentFiles.flatten.count(elem => elem == '+')
        val nbDeletions = differentFiles.flatten.count(elem => elem == '-')

        println(
          s"${Console.YELLOW}  commit " +
            commit.commitHash + Console.RESET +
            " (" + elem._1 + ")" + "\n" +
            "  Date:  " + commit.date + "\n" +
            newEmptyFiles.map(elem => "  " + elem + " | 0").mkString("\n") + "\n" +
            differentFiles.mkString("\n") + "\n" +
            "  " + nbFilesChanged + " file(s) changed, " + nbInsertions + " insertion(s)(+), " + nbDeletions + " deletion(s)(-)" + "\n"
        )
      })
    })
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
    if (commit1 == "None") { //Initial commit
      val commitFilesContent2 = recreateTree(rootPath, commit2).map(file => (file.split(" ")(2), getContentFile(rootPath + File.separator + ".sgit" + File.separator + "Blobs" + File.separator + file.split(" ")(1)))).toMap
      commitFilesContent2.filter(elem => elem._2 != "").map(elem => (elem._1, elem._2.split("\n").map(difference => "+ " + difference).toList))
    }
    else {
      val commitFilesContent1 = recreateTree(rootPath, commit1).map(file => (file.split(" ")(2), getContentFile(rootPath + File.separator + ".sgit" + File.separator + "Blobs" + File.separator + file.split(" ")(1)))).toMap
      val commitFilesContent2 = recreateTree(rootPath, commit2).map(file => (file.split(" ")(2), getContentFile(rootPath + File.separator + ".sgit" + File.separator + "Blobs" + File.separator + file.split(" ")(1)))).toMap

      //Those who are in commitFilesContent2 but not in commitFilesContent1
      val filesNew = commitFilesContent2.filter(elem => !commitFilesContent1.keys.toList.contains(elem._1)).filter(elem => elem._2 != "") //we get the new files not empty
      val newFilesAdditions = filesNew.toList.map(elem => (elem._1, elem._2.split("\n").map(add => "+ " + add).toList)) //as it is new files, we have only +

      val diffFiles = commitFilesContent2.filter(elem => !newFiles(rootPath, commit1, commit2).contains(elem._1))

      val diffFilesAdditionsDeletions = diffFiles.toList.map(elem => {
        val file = elem._1
        val content1 = commitFilesContent1.getOrElse(file, "").split("\n").toList
        val content2 = elem._2.split("\n").toList
        val matrix = mostLargestCommonSubSetMatrix(content1, content2, 0, 0, Map())
        val listDifferences = getDifferences(content1, content2, content1.length - 1, content2.length - 1, matrix, List())
        (file, listDifferences)
      })

      (diffFilesAdditionsDeletions ++ newFilesAdditions).filter(elem => elem._2.nonEmpty).toMap
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
  def newFiles(rootPath: String, commit1: String, commit2: String): List[String] = {
    if (commit1 == "None") recreateTree(rootPath, commit2).map(file => file.split(" ")(2)) //Initial commit
    else {
      val commitFiles1 = recreateTree(rootPath, commit1).map(file => (file.split(" ")(2)))
      val commitFiles2 = recreateTree(rootPath, commit2).map(file => (file.split(" ")(2)))
      commitFiles2.diff(commitFiles1)
    }
  }

}

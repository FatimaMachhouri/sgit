package command

import java.io.File
import utils.FileIO.getCurrentBranch
import utils.Path.getFilesDirectory
import utils.FileIO.getContentFile
import utils.Hash.encryptThisString

object Status {

  /**
   *
   * @param rootPath
   * @return a string
   * Returns the status of the sgit repository : current branch, untracked files, tracked files not committed, files updated
   */
  def status(rootPath: String): String = {
    val stageContent = getContentFile(rootPath + File.separator + ".sgit" + File.separator + "STAGE")
    val commitStageContent = getContentFile(rootPath + File.separator + ".sgit" + File.separator + "STAGECOMMIT")
    val filesInCurrentDirectory = getFilesDirectory(rootPath)

    val changesToBeCommitted = {
      val changes = getChangesToBeCommitted(rootPath, stageContent, commitStageContent)
      if (changes.isEmpty) ""
      else {
        "Changes to be committed:\n" +
          changes.map(elem => s"   ${Console.GREEN}" + "       " + elem + Console.RESET).mkString("\n")
      }
    }

    val modifiedFiles = {
      val getModified = getModifiedFiles(rootPath, stageContent, filesInCurrentDirectory)
      if (getModified.isEmpty) ""
      else {
        "Changes not staged for commit\n" + "  (use 'sgit add <file>...' to update what will be committed)\n" +
          getModified.map(elem => s"   ${Console.RED}" + "       " + elem + Console.RESET).mkString("\n")
      }
    }

    val untrackedFiles = {
      val getUntracked = getUntrackedFiles(rootPath, stageContent, filesInCurrentDirectory)
      if (getUntracked.isEmpty) ""
      else {
        "Untracked files:\n" + "  (use 'sgit add <file>...' to include in what will be committed)\n" +
          getUntracked.map(elem => s"   ${Console.RED}" + "       " + elem + Console.RESET).mkString("\n")
      }
    }

    prettyPrint(List(getCurrentBranch(rootPath), changesToBeCommitted, modifiedFiles, untrackedFiles))
  }


  /**
   *
   * @param stringsToPrint
   * @return String
   */
  def prettyPrint(stringsToPrint: List[String]): String = {
    if (stringsToPrint.isEmpty) ""
    else if (stringsToPrint.head.nonEmpty) stringsToPrint.head + "\n\n" + prettyPrint(stringsToPrint.tail)
    else prettyPrint(stringsToPrint.tail)
  }


  /**
   *
   * @param rootPath
   * @param stageContent
   * @param filesInCurrentDirectory
   * @return List[String]
   * Returns untracked files ie files present in the directory but not in the stage file
   */
  def getUntrackedFiles(rootPath: String, stageContent: String, filesInCurrentDirectory: List[String]): List[String] = {
    val filesInCurrentDirectoryTab = filesInCurrentDirectory.map(elem => elem.replace(rootPath + File.separator, ""))

    if (stageContent.isEmpty) filesInCurrentDirectoryTab
    else {
      val stagedFiles = {
        val stageContentTab = stageContent.split("\n")
        stageContentTab.map(elem => elem.split(" ")(2))
      }
      filesInCurrentDirectoryTab.diff(stagedFiles.toList)
    }
  }


  /**
   *
   * @param rootPath String
   * @param stageContent String
   * @param filesInCurrentDirectory List[String]
   * @return List[String]
   * Returns modified files ie files present in the stage file and in the directory but having different hash
   */
  def getModifiedFiles(rootPath: String, stageContent: String, filesInCurrentDirectory: List[String]): List[String] = {
    val listHashAndFilesDirectory = filesInCurrentDirectory.map(file => List(encryptThisString(getContentFile(file)), file.replace(rootPath + File.separator, "")))

    val stageContentTab = stageContent.split("\n")

    if (stageContent.isEmpty) List()
    else {
      val listHashAndFilesStage = stageContentTab.map(elem => List(elem.split(" ")(1), elem.split(" ")(2)))
      listHashAndFilesStage.diff(listHashAndFilesDirectory).map(elem => elem(1)).distinct.toList
    }
  }


  /**
   *
   * @param rootPath String
   * @param stageContent String
   * @param commitStageContent String
   * @return List[String]
   * Returns the staged files which are not in the last commit and the modified files (different between the stage and the last commit)
   */
  def getChangesToBeCommitted(rootPath: String, stageContent: String, commitStageContent: String): List[String] = {
    val stageContentTab = stageContent.split("\n")
    val lastCommitStageTab = commitStageContent.split("\n")

    if (stageContent.isEmpty) List()
    else if (commitStageContent.isEmpty) stageContentTab.map(elem => "new file:   " + elem.split(" ")(2)).toList
    else {
      //We get the new files
      val filesStage = stageContentTab.map(elem => elem.split(" ")(2))
      val filesCommit = lastCommitStageTab.map(elem => elem.split(" ")(2))
      val newFiles = filesStage.diff(filesCommit)
      val newFilesFormat = newFiles.map(file => "new file:   " + file)

      //We get the modified files (different hash)
      val newAndModifiedFiles = stageContentTab.diff(lastCommitStageTab).map(elem => elem.split(" ")(2))
      val modifiedFiles = newAndModifiedFiles.diff(newFiles)
      val modifiedFilesFormat = modifiedFiles.map(file => "modified:   " + file)

      (modifiedFilesFormat ++ newFilesFormat).toList
    }
  }

}

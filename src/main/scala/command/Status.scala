package command

import java.io.File
import utils.FileIO.getCurrentBranch
import utils.Path.{getFilesDirectory}
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

    val untrackedFiles = {
      val getUntracked = getUntrackedFiles(rootPath)
      if (getUntracked.isEmpty) ""
      else {
        "Untracked files:\n" + "  (use 'git add <file>...' to include in what will be committed)\n" +
          getUntracked.map(elem => "          " + elem).mkString("\n")
      }
    }

    val modifiedFiles = {
      val getModified = getModifiedFiles(rootPath)
      if (getModified.isEmpty) ""
      else {
        "Changes not staged for commit\n" + "  (use 'git add <file>...' to update what will be committed)\n" +
        getModified.map(elem => "          modified: " + elem).mkString("\n")
      }
    }

    val newFiles = {
      val changes = getChangesToBeCommitted(rootPath)
      if (changes.isEmpty) ""
      else {
        "Changes to be committed:\n" +
        changes.map(elem => "          TEST123: " + elem).mkString("\n")
      }
    }

    getBranch(rootPath) + "\n\n" + untrackedFiles + "\n\n" + modifiedFiles + "\n\n" + newFiles
  }


  /**
   *
   * @param rootPath
   * @return a string
   */
  private def getBranch(rootPath: String): String = {
    "On branch " + getCurrentBranch(rootPath)
  }


  /**
   *
   * @param rootPath
   * @return a string
   * Returns untracked files ie files present in the directory but not in the stage file
   */
  private def getUntrackedFiles(rootPath: String): List[String] = {
    val filesInCurrentDirectory = getFilesDirectory(rootPath).map(elem => elem.replace(rootPath + File.separator, ""))
    val stageContent = getContentFile(rootPath + File.separator + ".sgit" + File.separator + "STAGE")

    if (stageContent == "") filesInCurrentDirectory
    else {
      val stagedFiles = {
        val stageContentTab = stageContent.split("\n")
        stageContentTab.map(elem => elem.split(" ")(2))
      }
      filesInCurrentDirectory.diff(stagedFiles.toList)
    }
  }


  /**
   *
   * @param rootPath string
   * @return an array of string
   * Returns modified files ie files present in the stage file and in the directory but having different hash
   */
  def getModifiedFiles(rootPath: String): Array[String] = {
    val filesInCurrentDirectory = getFilesDirectory(rootPath)
    val listHashAndFilesDirectory = filesInCurrentDirectory.map(file => List(encryptThisString(getContentFile(file)), file.replace(rootPath + File.separator, "")))

    val stageContent = getContentFile(rootPath + File.separator + ".sgit" + File.separator + "STAGE")
    val stageContentTab = stageContent.split("\n")

    if (stageContent == "") Array()
    else {
      val listHashAndFilesStage = stageContentTab.map(elem => List(elem.split(" ")(1), elem.split(" ")(2)))
      listHashAndFilesStage.diff(listHashAndFilesDirectory).map(elem => elem(1)).distinct
    }
  }


  /**
   *
   * @param rootPath
   * @return a string
   * Returns the staged files which are not in the last commit and the modified files (modified between the stage and the last commit)
   */
  private def getChangesToBeCommitted(rootPath: String): List[String] = {
    List()
  }
}

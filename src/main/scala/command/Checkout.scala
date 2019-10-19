package command

import java.io.File
import utils.FileIO.{getContentFile, recreateTree, writeInFile}
import utils.Hash.encryptThisString
import utils.Path.getFilesDirectory


object Checkout {

  def checkout(rootPath: String, branchName: String): Boolean = {
    val existantBranches = getFilesDirectory(rootPath + File.separator + ".sgit" + File.separator + "Branches").map(branch => branch.replace(rootPath + File.separator + ".sgit" + File.separator + "Branches" + File.separator, ""))

    if (!existantBranches.contains(branchName) || getContentFile(rootPath + File.separator + ".sgit" + File.separator + "Commits").isEmpty) false
    else {
      val currentBranch = getContentFile(rootPath + File.separator + ".sgit" + "HEAD")
      val currentCommit = getContentFile(rootPath + File.separator + ".sgit" + File.separator + "Branches" + File.separator + currentBranch)

      val targetCommit = getContentFile(rootPath + File.separator + ".sgit" + File.separator + "Branches" + File.separator + branchName)

      val areEqualsCommit = recreateTree(rootPath, currentCommit).equals(recreateTree(rootPath, targetCommit))

      if (areEqualsCommit) {
        writeInFile(rootPath + File.separator + ".sgit" + "HEAD", branchName)
        true
      }
      else {
        val changesToBeCommitted = getChangesToBeCommitted(rootPath).filter(elem => elem.contains("modified"))
        val changesNoStagedForCommit = getModifiedFiles(rootPath)
        if (changesNoStagedForCommit.nonEmpty || changesToBeCommitted.nonEmpty) false
        else {
          val untracked = getUntrackedFiles(rootPath)

          true
        }
      }
    }

  }


  /**
   *
   * @param rootPath
   * @return an array of string
   * Returns the staged files which are not in the last commit and the modified files (different between the stage and the last commit)
   */
  def getChangesToBeCommitted(rootPath: String): Array[String] = {
    val stageContent = getContentFile(rootPath + File.separator + ".sgit" + File.separator + "STAGE")
    val commitStageContent = getContentFile(rootPath + File.separator + ".sgit" + File.separator + "STAGECOMMIT")

    val stageContentTab = stageContent.split("\n")
    val lastCommitStageTab = commitStageContent.split("\n")

    if (stageContent == "") Array()
    else if (commitStageContent == "") stageContentTab.map(elem => "new file:   " + elem.split(" ")(2))
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

      modifiedFilesFormat ++ newFilesFormat
    }
  }


  /**
   *
   * @param rootPath
   * @return a string
   * Returns untracked files and their content (ie files present in the directory but not in the stage file)
   */
  def getUntrackedFiles(rootPath: String): Map[String, String] = {
    val filesInCurrentDirectory = getFilesDirectory(rootPath).map(elem => elem.replace(rootPath + File.separator, ""))
    val stageContent = getContentFile(rootPath + File.separator + ".sgit" + File.separator + "STAGE")

    if (stageContent == "") filesInCurrentDirectory.map(file => (file, getContentFile(file))).toMap
    else {
      val stagedFiles = {
        val stageContentTab = stageContent.split("\n")
        stageContentTab.map(elem => elem.split(" ")(2))
      }
      filesInCurrentDirectory.diff(stagedFiles.toList).map(file => (file, getContentFile(file))).toMap
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

}

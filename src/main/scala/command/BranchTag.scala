package command

import java.io.File
import utils.FileIO.{createFile, getContentFile, writeInFile}
import utils.Path.getFilesDirectory

object BranchTag {

  /**
   *
   * @param rootPath
   * @param branchName
   */
  def createBranch(rootPath: String, branchName: String): Unit = {
    val currentBranch = getContentFile(rootPath + File.separator + ".sgit" + File.separator + "HEAD")
    val lastCommit = getContentFile(rootPath + File.separator + ".sgit" + File.separator + "Branches" + File.separator + currentBranch)
    createFile(rootPath + File.separator + ".sgit" + File.separator + "Branches" + File.separator + branchName)
    writeInFile(rootPath + File.separator + ".sgit" + File.separator + "Branches" + File.separator + branchName, lastCommit)
  }


  /**
   *
   * @param rootPath
   * @param tag
   */
  def createTag(rootPath: String, tag: String): Unit = {
    val currentBranch = getContentFile(rootPath + File.separator + ".sgit" + File.separator + "HEAD")
    val lastCommit = getContentFile(rootPath + File.separator + ".sgit" + File.separator + "Branches" + File.separator + currentBranch)
    createFile(rootPath + File.separator + ".sgit" + File.separator + "Tags" + File.separator + tag)
    writeInFile(rootPath + File.separator + ".sgit" + File.separator + "Tags" + File.separator + tag, lastCommit)
  }


  /**
   *
   * @param rootPath
   * @return List[String]
   * Returns branches with their last commit in the format : Branch LastCommitHash
   */
  def listBranches(rootPath: String): List[String] = {
    val branchesNames = getFilesDirectory(rootPath + File.separator + ".sgit" + File.separator + "Branches").map(elem => elem.replace(rootPath + File.separator + ".sgit" + File.separator + "Branches" + File.separator, ""))
    branchesNames.map(branch => branch + " " + getContentFile(rootPath + File.separator + ".sgit" + File.separator + "Branches" + File.separator + branch))
  }


  /**
   *
   * @param rootPath
   * @return List[String]
   * Returns tags in the format : Tag CommitHash
   */
  def listTags(rootPath: String): List[String] = {
    val tagsNames = getFilesDirectory(rootPath + File.separator + ".sgit" + File.separator + "Tags").map(elem => elem.replace(rootPath + File.separator + ".sgit" + File.separator + "Tags" + File.separator, ""))
    tagsNames.map(tag => tag + " " + getContentFile(rootPath + File.separator + ".sgit" + File.separator + "Tags" + File.separator + tag))
  }

}

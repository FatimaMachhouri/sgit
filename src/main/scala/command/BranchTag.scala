package command

import java.io.File
import utils.FileIO.{createFile, getContentFile, writeInFile}
import utils.Path.getFilesDirectory

object BranchTag {

  /**
   *
   * @param rootPath
   * @param branchName
   * @return Boolean
   * //We can't create a branch if number of commits == 0
   */
  def createBranch(rootPath: String, branchName: String): Boolean = {
    val currentBranch = getContentFile(rootPath + File.separator + ".sgit" + File.separator + "HEAD")
    val nbCommits = getFilesDirectory(rootPath + File.separator + ".sgit" + File.separator + "Commits")
    if (nbCommits.nonEmpty) {
      val lastCommit = getContentFile(rootPath + File.separator + ".sgit" + File.separator + "Branches" + File.separator + currentBranch)
      createFile(rootPath + File.separator + ".sgit" + File.separator + "Branches" + File.separator + branchName)
      writeInFile(rootPath + File.separator + ".sgit" + File.separator + "Branches" + File.separator + branchName, lastCommit)
      true
    }
    else false
  }


  /**
   *
   * @param rootPath
   * @param tag
   * @return Boolean
   * //We can't create a tag if number of commits == 0
   */
  def createTag(rootPath: String, tag: String): Boolean = {
    val currentBranch = getContentFile(rootPath + File.separator + ".sgit" + File.separator + "HEAD")
    val nbCommits = getFilesDirectory(rootPath + File.separator + ".sgit" + File.separator + "Commits")
    if (nbCommits.nonEmpty) {
      val lastCommit = getContentFile(rootPath + File.separator + ".sgit" + File.separator + "Branches" + File.separator + currentBranch)
      createFile(rootPath + File.separator + ".sgit" + File.separator + "Tags" + File.separator + tag)
      writeInFile(rootPath + File.separator + ".sgit" + File.separator + "Tags" + File.separator + tag, lastCommit)
      true
    }
    else false
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

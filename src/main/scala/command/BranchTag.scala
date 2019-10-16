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
   */
  def listBranches(rootPath: String): List[String] = {
    getFilesDirectory(rootPath + File.separator + "Branches")
  }


  /**
   *
   * @param rootPath
   * @return List[String]
   */
  def listTags(rootPath: String): List[String] = {
    getFilesDirectory(rootPath + File.separator + "Tags")
  }

}

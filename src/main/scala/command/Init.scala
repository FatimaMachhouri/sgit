package command

import java.io.File
import utils.FileIO.{createDirectory, createFile, writeInFile}
import utils.Path.getContentDirectory

object Init {

  /**
   *
   * @param rootPath String
   * @return a Boolean
   *
   * Initialize the current directory as a sgit repository.
   * Can be used to convert an existing unversioned project to a sgit repository or initialize a new empty repository.
   * If the directory is already a sgit repository (contains a .sgit directory), returns false otherwise creates HEAD and STAGE files and Blobs, Branches, Commits, Logs, Tags and Trees directories in .sgit directory in the path parameter directory.
   */
  def init(rootPath: String): Boolean = {
    val contentDirectory = getContentDirectory(rootPath)

    if (isAlreadySgitRepository(rootPath, contentDirectory)) false

    else {
      val files: List[String] = List("HEAD", "STAGE", "STAGECOMMIT")
      val folders: List[String] = List("Blobs", "Branches", "Commits", "Logs", "Tags", "Trees")

      val newPath = {
        rootPath match {
          case "" => ".sgit"
          case _ => rootPath + File.separator + ".sgit"
        }
      }

      createDirectory(newPath)

      folders.map(folder => createDirectory(newPath + File.separator + folder))
      files.map(file => createFile(newPath + File.separator + file))

      //We write in the stage file the initial branch name ie master
      writeInFile(newPath + File.separator + "HEAD", "master")

      true
    }
  }


  /**
   *
   * @param rootPath String
   * @param contentDirectory List[String]
   * @return a Boolean
   * Returns true if the path directory contains a .sgit ie is a SGit Repository otherwise false.
   */
  private def isAlreadySgitRepository(rootPath: String, contentDirectory: List[String]): Boolean = {
    contentDirectory.contains(rootPath + File.separator + ".sgit")
  }

}
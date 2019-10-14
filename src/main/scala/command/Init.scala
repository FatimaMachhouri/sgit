package command

import java.io.File
import utils.FileIO.{createDirectory, createFile, writeInFile}

object Init {

  /**
   *
   * @return a boolean
   *
   * Initialize the current directory as a SGit Repository.
   * Can be used to convert an existing unversioned project to a SGit repository or initialize a new empty repository.
   * If the directory is already a SGit Repository (contains a .sgit directory), returns false otherwise creates HEAD and STAGE files and Blobs, Branches, Commits, Logs, Tags and Trees directories in .sgit directory in the path parameter directory.
   */
  def init(rootPath: String): Boolean = {
    if (isAlreadySgitRepository(rootPath)) false

    else {
      val files: List[String] = List("HEAD", "STAGE", "STAGECOMMIT")
      val folders: List[String] = List("Blobs", "Branches", "Commits", "Logs", "Tags", "Trees")

      val newPath = if (rootPath != "") rootPath + File.separator + ".sgit" else ".sgit"
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
   * @return a boolean
   * Returns true if the path directory contains a .sgit ie is a SGit Repository otherwise false.
   */
  private def isAlreadySgitRepository(rootPath: String): Boolean = {
    //We get the current repository
    val currentDirectory = new File(rootPath + File.separator + ".sgit")
    currentDirectory.exists()
  }

}

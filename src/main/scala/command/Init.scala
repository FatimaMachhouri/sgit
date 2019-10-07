package command

import java.io.{BufferedWriter, File, FileWriter}

class Init {

  /*
    Returns a boolean

    Initialize the current directory as a SGit Repository.
    Can be used to convert an existing unversioned project to a SGit repository or initialize a new empty repository.

    If the directory is already a SGit Repository (contains a .sgit directory), returns false otherwise creates HEAD and STAGE files and Blobs, Branches, Commits, Tags and Trees directories in .sgit directory in the path parameter directory.
   */
  def init(): Boolean = {
    if (isAlreadySgitRepository()) false

    else {
      val files: List[String] = List("HEAD", "STAGE")
      val folders: List[String] = List("Blobs", "Branches", "Commits", "Tags", "Trees")

      val newPath = ".sgit"

      val sgitFolder = new File(newPath).mkdir()
      folders.map(folder => new File(newPath + File.separator + folder).mkdir)
      files.map(file => new File(newPath + File.separator + file).createNewFile())

      val file = new File(newPath + File.separator + "HEAD")
      val bw = new BufferedWriter(new FileWriter(file))
      bw.write("master")
      bw.close()

      true
    }
  }


  /*
    Returns a boolean

    Returns true if the path directory contains a .sgit ie is a SGit Repository otherwise false.
   */
  private def isAlreadySgitRepository(): Boolean = {
    //We get the current repository
    val currentPath = new File(".").getCanonicalPath
    val currentDirectory = new File(currentPath + File.separator + ".sgit")
    currentDirectory.exists()
  }

}

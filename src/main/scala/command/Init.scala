package command

import java.io.{BufferedWriter, File, FileWriter}

class Init {

  /*
    The function takes in parameter a String and returns a Boolean.

    Initialize the path parameter directory as a SGit Repository.
    Can be used to convert an existing unversioned project to a SGit repository or initialize a new empty repository.

    If the directory is already a SGit Repository (contains a .sgit directory), returns false otherwise creates HEAD and STAGE files and Blobs, Branches, Commits, Tags and Trees directories in .sgit directory in the path parameter directory.

   */
  def init(path: String): Boolean = {

    if (isAlreadySgitRepository(path)) false

    else {
      val files: List[String] = List("HEAD", "STAGE")
      val folders: List[String] = List("Blobs", "Branches", "Commits", "Tags", "Trees")

      val newPath = path + File.separator + ".sgit"

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
    The function takes in parameter a String and returns a Boolean.
    Returns true if the path directory contains a .sgit ie is a SGit Repository otherwise false.
   */
  private def isAlreadySgitRepository(path: String): Boolean = {
    val currentDirectory = new File(path + File.separator + ".sgit")
    currentDirectory.exists()
  }

}

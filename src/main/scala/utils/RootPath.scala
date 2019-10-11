package utils

import java.io.File

import scala.annotation.tailrec

object RootPath {

  /**
   *
   * @return a string
   * Returns the parent directory of the .sgit directory ie the root directory
   * If the .sgit not found, returns an empty string ""
   */
  def sgitParentPath(): String = {
    val currentRepositoryPath = new File(".").getCanonicalPath
    val currentRepositoryPathTab = currentRepositoryPath.split(File.separator)

    @tailrec
    def sgitParentPathTailRec(directory: Array[String]): String = {
      val directorySgit = new File(directory.mkString(File.separator) + File.separator + ".sgit")
      if (directory.length == 0) ""
      else if (directorySgit.exists() & directorySgit.isDirectory) directory.mkString(File.separator)
      else sgitParentPathTailRec(directory.dropRight(1))
    }

    sgitParentPathTailRec(currentRepositoryPathTab)
  }

}

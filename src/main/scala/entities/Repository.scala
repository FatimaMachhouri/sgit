package entities

import java.io.File
import scala.annotation.tailrec

object Repository {

  /**
   *
   * @return a boolean
   * True if the repository contains a .sgit or if its parent directories contain a .sgit
   */
  def isASgitRepository(path: String): Boolean = {
    val currentRepositoryPathTab = path.split(File.separator)

    @tailrec
    def isASgitRepositoryTailRec(directory: Array[String]): Boolean = {
      val directorySgit = new File(directory.mkString(File.separator) + File.separator + ".sgit")
      if (directory.length == 0) false
      else if (directorySgit.exists() & directorySgit.isDirectory) true
      else isASgitRepositoryTailRec(directory.dropRight(1))
    }

    isASgitRepositoryTailRec(currentRepositoryPathTab)
  }


}

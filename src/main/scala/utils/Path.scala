package utils

import java.io.File
import scala.annotation.tailrec

object Path {

  /**
   *
   * @param path String
   * @return String
   * Returns the parent directory of the .sgit directory ie the root directory
   * If the .sgit not found, returns an empty string ""
   */
  def sgitParentPath(path: String): String = {
    val currentRepositoryPathTab = path.split(File.separator)

    @tailrec
    def sgitParentPathTailRec(directory: Array[String]): String = {
      val directorySgit = new File(directory.mkString(File.separator) + File.separator + ".sgit")
      if (directory.length == 0) ""
      else if (directorySgit.exists() & directorySgit.isDirectory) directory.mkString(File.separator)
      else sgitParentPathTailRec(directory.dropRight(1))
    }

    sgitParentPathTailRec(currentRepositoryPathTab)
  }


  /**
   *
   * @param path String
   * @return List[String]
   * Returns all the content (files and sub-directories) of a directory
   */
  def getContentDirectory(path: String): List[String] = {

    val directory = new File(path)
    val directoryListElems = directory.listFiles()

    @tailrec
    def getContentDirectoryTailRec(rootContent: Array[File], acc: List[File]): List[File] = {
      if (rootContent.isEmpty) acc

      else if (rootContent.head.isFile) {
        getContentDirectoryTailRec(rootContent.tail, rootContent.head :: acc)
      }

      else {
        val subDirectory = new File(rootContent.head.toString).listFiles()
        getContentDirectoryTailRec(rootContent.tail ++ subDirectory, rootContent.head :: acc)
      }
    }

    getContentDirectoryTailRec(directoryListElems, List()).map(elem => elem.toString)
  }


  /**
   *
   * @param path String
   * @return List[String]
   * Returns all the content files (and sub files) of a directory
   * Exclude the .sgit directory
   */
  def getFilesDirectory(path: String): List[String] = {

    val directory = new File(path)
    val directoryListElems = directory.listFiles()

    @tailrec
    def getFilesDirectoryTailRec(rootContent: Array[File], acc: List[File]): List[File] = {
      if (rootContent.isEmpty) acc

      else if (rootContent.head.isFile) {
        getFilesDirectoryTailRec(rootContent.tail, rootContent.head :: acc)
      }

      else if (rootContent.head.toString != path + File.separator + ".sgit") { //we exclude the .sgit directory
        val subDirectory = new File(rootContent.head.toString).listFiles()
        getFilesDirectoryTailRec(rootContent.tail ++ subDirectory, acc)
      }

      else {
        getFilesDirectoryTailRec(rootContent.tail, acc)
      }
    }

    getFilesDirectoryTailRec(directoryListElems, List()).map(elem => elem.toString)
  }

}

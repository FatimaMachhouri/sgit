package utils

import java.io.File

import scala.annotation.tailrec

object Path {

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


  /**
   *
   * @param rootContent Array[File]
   * @param acc List[File]
   * @return a list of file
   * Returns the content (files and sub-directories) of a directory
   * If the path directory that we want to list the content is "path", rootContent is "new File(path).listFiles"
   * acc is originally an empty list
   */
  @tailrec
  def contentDirectoryTailRec(rootContent: Array[File], acc: List[File]): List[File] = {
    if (rootContent.isEmpty) acc

    else if (rootContent.head.isFile) {
      contentDirectoryTailRec(rootContent.tail, rootContent.head :: acc)
    }

    else {
      val subDirectory = new File(rootContent.head.toString).listFiles()
      contentDirectoryTailRec(rootContent.tail ++ subDirectory, rootContent.head :: acc)
    }
  }

}

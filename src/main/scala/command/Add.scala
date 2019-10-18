package command

import java.io.{File}
import utils.Hash.encryptThisString
import utils.FileIO.{getContentFile, createFile, writeInFile, isADirectory}
import utils.Path.getFilesDirectory

object Add {

  /**
   * @param rootPath String
   * @param currentPath String
   * @param filePaths List[String]
   *
   * Creates a Blob File for each file of the list parameter in the currentPath/.sgit/Blobs with an encrypted name based on the SHA-1
   */
  def add(rootPath: String, currentPath: String, filePaths: List[String]): Unit = {
    val pathBlobs = rootPath + File.separator + ".sgit" + File.separator + "Blobs"

    val directories = filePaths.filter(elem => isADirectory(elem))
    val directoriesFiles = directories.map(elem => getFilesDirectory(elem))
    val files = filePaths.filter(elem => !isADirectory(elem))

    val allFiles = List.concat(files, directoriesFiles.flatten)

    allFiles.map(file => {
      //Step 0 : if we used . to add, we have to re-format the paths
      val formatFile = if (file.contains("./")) file.replace("./", "") else file

      //Step 1 : We crypt the file content
      val fileContent = getContentFile(formatFile)
      val cryptedContent = encryptThisString(fileContent)

      //Step 2 : We create the blob file with a crypted name based on the content
      createFile(pathBlobs + File.separator + cryptedContent)

      //Step 3 : We write the file content in the blob
      writeInFile(pathBlobs + File.separator + cryptedContent, fileContent)

      //Step 4 : We get the stage content
      val stage = rootPath + File.separator + ".sgit" + File.separator + "STAGE"
      val stageContent = getContentFile(stage)

      //Step 5 : We check if the file is already staged in order to not stage it twice
      val newStageContent = {
        val relativePath = (currentPath + File.separator + formatFile).replace(rootPath + "/", "")

        val contentStage = getContentFile(rootPath + File.separator + ".sgit" + File.separator + "STAGE")

        if (alreadyStaged(rootPath, currentPath, contentStage, formatFile)) {
          val stageContentWithoutFile = stageContent.split("\n").filter(_.split(" ")(2) != relativePath)
          val newStage = Array("Blob " + cryptedContent + " " + relativePath) ++ stageContentWithoutFile
          newStage.mkString("\n")
        }
        else if (stageContent == "") "Blob " + cryptedContent + " " + relativePath
        else stageContent + "\n" + "Blob " + cryptedContent + " " + relativePath
      }

      //Step 6 : We write the content in the stage file
      writeInFile(rootPath + File.separator + ".sgit" + File.separator + "STAGE", newStageContent.toString)
    })
  }

  /**
   *
   * @param file
   * @return a boolean
   * Return true if the file is already in the stage else false
   */
  private def alreadyStaged(rootPath: String, currentPath: String, stageContent: String, file: String): Boolean = {
    stageContent.contains((currentPath + File.separator + file).replace(rootPath + "/", ""))
  }

}

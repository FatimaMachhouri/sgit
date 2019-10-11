package command

import java.io.{File}
import utils.Hash.encryptThisString
import utils.FileIO.{getContentFile, createFile, writeInFile}

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

    filePaths.foreach(file => {
      //Step 1 : We crypt the file content
      val fileContent = getContentFile(file)
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
        val relativePath = (currentPath + File.separator + file).replace(rootPath + "/", "")

        if (alreadyStaged(rootPath, currentPath, file)) {
          val stageContentWithoutFile = stageContent.split("\n").filter(!_.contains(relativePath))
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
  private def alreadyStaged(rootPath: String, currentPath: String, file: String): Boolean = {
    val path = rootPath + File.separator + ".sgit" + File.separator + "STAGE"
    val stageContent = getContentFile(path)
    stageContent.contains((currentPath + File.separator + file).replace(rootPath + "/", ""))
  }

}

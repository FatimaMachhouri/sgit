package command

import java.io.{BufferedWriter, File, FileWriter}
import utils.Hash.encryptThisString
import scala.io.Source

object Add {

  /**
   *
   * @param filePaths List[String]
   *
   * Creates a Blob File for each file of the list parameter in the currentPath/.sgit/Blobs with an encrypted name based on the SHA-1
   */
  def add(filePaths: List[String]): Unit = {
    val currentRepositoryPath = new File(".").getCanonicalPath
    val pathBlobs = currentRepositoryPath + File.separator + ".sgit/Blobs"

    filePaths.map(file => {
      //Step 1 : We crypt the file content
      val fileContent = Source.fromFile(file).mkString
      val cryptedContent = encryptThisString(fileContent)

      //Step 2 : We create the blob file with a crypted name based on the content
      val blobFile = new File(pathBlobs + File.separator + cryptedContent)
      blobFile.createNewFile()

      //Step 3 : We write the file content in the blob
      val bw = new BufferedWriter(new FileWriter(blobFile))
      bw.write(fileContent)
      bw.close()

      //Step 4 : We get the stage content
      val stage = new File(".").getCanonicalPath + File.separator + ".sgit" + File.separator + "STAGE"
      val stageContent = Source.fromFile(stage).mkString

      //Step 5 : We check if the file is already staged
      val newStageContent = {
        if (alreadyStaged(file)) {
          val stageContentWithoutFile = stageContent.split("\n").filter(!_.contains(file))
          stageContentWithoutFile.mkString("\n") + "\n" + cryptedContent + " " + file
        }
        else if (stageContent == "") cryptedContent + " " + file
        else stageContent + "\n" + cryptedContent + " " + file
      }

      //Step 6 : We write the content in the stage file
      val bws = new BufferedWriter(new FileWriter(currentRepositoryPath + File.separator + ".sgit" + File.separator + "STAGE"))
      bws.write(newStageContent.toString)
      bws.close()

    })
  }

  /**
   *
   * @param file
   * @return boolean
   * Return true if the file is already in the stage else false
   */
  private def alreadyStaged(file: String): Boolean = {
    val path = new File(".").getCanonicalPath + File.separator + ".sgit" + File.separator + "STAGE"
    val stageContent = Source.fromFile(path).mkString
    stageContent.contains(file)
  }

}

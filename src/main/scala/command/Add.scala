package command

import java.io.{BufferedWriter, File, FileWriter}
import utils.Hash.encryptThisString
import scala.io.Source

object Add {

  /**
   *
   * @param filePaths List[String]
   *
   * Creates a Blob File for each file of the list parameter in the currentPath with an encrypted name based on the SHA-1
   */
  def add(filePaths: List[String]): Unit = {
    val currentRepositoryPath = new File(".").getCanonicalPath
    val pathBlobs = currentRepositoryPath + File.separator + ".sgit/Blobs"

    filePaths.map(file => {
      //Step 1 : We crypt the file content
      val fileContent = Source.fromFile(file).mkString
      val cryptedContent = encryptThisString(fileContent)

      //Step 2 : We create the blob file with a crypted name based on the content
      val blobFile = new File(pathBlobs + File.separator + encryptThisString(cryptedContent))
      blobFile.createNewFile()

      //Step 3 : We write the file content in the blob
      val bw = new BufferedWriter(new FileWriter(blobFile))
      bw.write(fileContent)
      bw.close()

      //Step 4 : We write in the stage file the arborescence created
      val bws = new BufferedWriter(new FileWriter(currentRepositoryPath + File.separator + ".sgit" + File.separator + "STAGE", true))
      bws.write(cryptedContent + " " + file)
      bws.newLine();
      bws.close()
    })
  }

}

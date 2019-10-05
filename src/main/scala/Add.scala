import java.io.{BufferedWriter, File, FileWriter}
import java.math.BigInteger
import java.security.MessageDigest
import java.security.NoSuchAlgorithmException
import scala.io.Source

class Add {

  /**
   *
   * @param repositoryPath String
   * @param filePaths List[String]
   *
   * Creates a Blob File for each file of the list parameter in the repositoryPath/.sgit/Blobs with an encrypted name based on the SHA-1
   */
  def add(repositoryPath: String, filePaths: List[String]): Unit = {
    val pathBlobs = repositoryPath + File.separator + ".sgit/Blobs"

    filePaths.map(file => {
      //Step 1 : We crypt the file content
      val fileContent = Source.fromFile(file).getLines.mkString
      val cryptedContent = encryptThisString(fileContent)

      //Step 2 : We create the blob file with a crypted name based on the content
      val blobFile = new File(pathBlobs + File.separator + encryptThisString(cryptedContent))
      blobFile.createNewFile()

      //Step 3 : We write the file content in the blob
      val bw = new BufferedWriter(new FileWriter(blobFile))
      bw.write(fileContent)
      bw.close()

    })
  }


  /**
   *
   * @param input
   * @return
   *
   * Encrypt the input parameter by using the Secure Hash Algorithm 1 (SHA-1)
   */
  private def encryptThisString(input: String): String = try {
    // getInstance() method is called with algorithm SHA-1
    val md = MessageDigest.getInstance("SHA-1")
    // digest() method is called to calculate message digest of the input string
    // returned as array of byte
    val messageDigest = md.digest(input.getBytes)
    // Convert byte array into signum representation
    val no = new BigInteger(1, messageDigest)
    // Convert message digest into hex value
    var hashtext = no.toString(16)
    // Add preceding 0s to make it 32 bit
    while ( {
      hashtext.length < 32
    }) hashtext = "0" + hashtext
    // return the HashText
    hashtext
  } catch {
    case e: NoSuchAlgorithmException =>
      throw new RuntimeException(e)
  } // For specifying wrong message digest algorithms

}
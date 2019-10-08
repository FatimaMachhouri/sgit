package utils

import java.math.BigInteger
import java.security.{MessageDigest, NoSuchAlgorithmException}

object Hash {

  /**
   *
   * @param input
   * @return
   *
   * Encrypt the input parameter by using the Secure Hash Algorithm 1 (SHA-1)
   */
  def encryptThisString(input: String): String = try {
    // getInstance() method is called with algorithm SHA-1
    val md = MessageDigest.getInstance("SHA-1")
    // digest() method is called to calculate message digest of the input string
    // returned as array of byte
    val messageDigest = md.digest(input.getBytes)
    // Convert byte array into signum representation
    val no = new BigInteger(1, messageDigest)
    // Convert message digest into hex value
    var hashtext = no.toString(16)
    // command.Add preceding 0s to make it 32 bit
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

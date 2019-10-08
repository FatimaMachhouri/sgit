package utils

import java.math.BigInteger
import java.security.{MessageDigest, NoSuchAlgorithmException}

object Hash {

  /**
   *
   * @param input
   * @return a String
   *
   * Encrypt the input parameter by using the Secure Hash Algorithm 1 (SHA-1)
   */
  def encryptThisString(input: String): String = try {
    val md = MessageDigest.getInstance("SHA-1")
    val messageDigest = md.digest(input.getBytes)
    val no = new BigInteger(1, messageDigest)
    var hashtext = no.toString(16)
    while ({
      hashtext.length < 32
    }) hashtext = "0" + hashtext
    hashtext
  } catch {
    case e: NoSuchAlgorithmException =>
      throw new RuntimeException(e)
  }
}

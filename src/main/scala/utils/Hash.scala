package utils

import java.math.BigInteger
import java.security.{MessageDigest, NoSuchAlgorithmException}

object Hash {

  /**
   *
   * @param input String
   * @return String
   *
   * Returns the input parameter encrypted by using the Secure Hash Algorithm 1 (SHA-1)
   */
  def encryptThisString(input: String): String = try {
    val md = MessageDigest.getInstance("SHA-1")
    val messageDigest = md.digest(input.getBytes)
    val no = new BigInteger(1, messageDigest)
    no.toString(16)
  } catch {
    case e: NoSuchAlgorithmException =>
      throw new RuntimeException(e)
  }
}

import org.scalatest.FlatSpec
import utils.Hash.encryptThisString

class HashTest extends FlatSpec {

  "HashTest" should "return the same hash for the same input" in {
    assert(encryptThisString("test hash").equals(encryptThisString("test hash")))
  }

  it should "return a different hash for a different input" in {
    assert(!encryptThisString("testhash").equals(encryptThisString("test hash")))
  }

}
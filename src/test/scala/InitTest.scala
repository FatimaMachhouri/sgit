import org.scalatest.{BeforeAndAfter, FlatSpec}
import command.Init
import scala.reflect.io.Directory
import java.io.File
import utils.FileIO.createDirectory

class InitTest extends FlatSpec with BeforeAndAfter {

  before {
    val initTestDirectory = new File(".").getCanonicalPath + File.separator + "InitDirectoryTest"
    createDirectory(initTestDirectory)
  }

  after {
    val initTestDirectory = new File(".").getCanonicalPath + File.separator + "InitDirectoryTest"
    val directory = new Directory(new File(initTestDirectory))
    directory.deleteRecursively()
  }

  "Init" should "create a .sgit in the path" in {
    val initTestDirectory = new File(".").getCanonicalPath + File.separator + "InitDirectoryTest"
    val init = Init
    assert(init.init(initTestDirectory))
  }

  it should "not create a .sgit if already exists in the path" in {
    val initTestDirectory = new File(".").getCanonicalPath + File.separator + "InitDirectoryTest"
    val init = Init
    init.init(initTestDirectory)
    assert(!init.init(initTestDirectory))
  }

}
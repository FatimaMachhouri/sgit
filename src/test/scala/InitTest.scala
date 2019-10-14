import org.scalatest.{BeforeAndAfter, FlatSpec}
import command.Init
import scala.reflect.io.Directory
import java.io.File
import utils.FileIO.createDirectory

class InitTest extends FlatSpec with BeforeAndAfter {

  before {
    val initDirectoryTest = new File(".").getCanonicalPath + File.separator + "InitDirectoryTest"
    createDirectory(initDirectoryTest)
  }

  after {
    val initDirectoryTest = new File(".").getCanonicalPath + File.separator + "InitDirectoryTest"
    val directory = new Directory(new File(initDirectoryTest))
    directory.deleteRecursively()
  }

  "Init" should "create a .sgit in the path" in {
    val init = Init
    val initDirectoryTest = new File(".").getCanonicalPath + File.separator + "InitDirectoryTest"
    assert(init.init(initDirectoryTest))
  }

  it should "not create a .sgit if already exists in the path" in {
    val init = Init
    val initDirectoryTest = new File(".").getCanonicalPath + File.separator + "InitDirectoryTest"
    init.init(initDirectoryTest)
    assert(!init.init(initDirectoryTest))
  }


}
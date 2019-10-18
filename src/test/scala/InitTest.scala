import org.scalatest.{BeforeAndAfter, FlatSpec}
import command.Init
import scala.reflect.io.Directory
import java.io.File
import utils.FileIO.createDirectory
import utils.Path.getContentDirectory


class InitTest extends FlatSpec with BeforeAndAfter {

  before {
    val initTestDirectory = new File(".").getCanonicalPath + File.separator + "InitDirectoryTest"
    createDirectory(initTestDirectory)
  }

  after {
    val initTestDirectory = new File(".").getCanonicalPath + File.separator + "InitDirectoryTest"
    val directory = new Directory(new File(initTestDirectory))
    directory.deleteRecursively()

    val sgitCurrentDirectoryPath = new File(".").getCanonicalPath + File.separator + ".sgit"
    val currentDirectory = new Directory(new File(sgitCurrentDirectoryPath))
    currentDirectory.deleteRecursively()
  }

  "Init" should "create a .sgit" in {
    val initTestDirectory = new File(".").getCanonicalPath + File.separator + "InitDirectoryTest"
    assert(Init.init(initTestDirectory))
  }

  it should "create a .sgit in the path parameter" in {
    val initTestDirectory = new File(".").getCanonicalPath + File.separator + "InitDirectoryTest"
    Init.init(initTestDirectory)
    assert(getContentDirectory(initTestDirectory).contains(initTestDirectory + File.separator + ".sgit"))
  }

  it should "not create a .sgit if already exists in the path" in {
    val initTestDirectory = new File(".").getCanonicalPath + File.separator + "InitDirectoryTest"
    Init.init(initTestDirectory)
    assert(!Init.init(initTestDirectory))
  }

  it should "create a .sgit in the path which contains Blobs, Branches, Commits, Logs, Tags, Trees directories and HEAD, STAGE and STAGECOMMIT files" in {
    val initTestDirectory = new File(".").getCanonicalPath + File.separator + "InitDirectoryTest"
    Init.init(initTestDirectory)
    assert(getContentDirectory(initTestDirectory + File.separator + ".sgit").length == 9)
  }

}
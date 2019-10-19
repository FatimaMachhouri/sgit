import org.scalatest.{BeforeAndAfter, FlatSpec}
import command.{Add, Commit, Init}
import scala.reflect.io.Directory
import java.io.File
import utils.FileIO.{createDirectory, createFile, getContentFile}
import utils.Path.getFilesDirectory

class CommitTest extends FlatSpec with BeforeAndAfter {

  before {
    val commitTestDirectory = new File(".").getCanonicalPath + File.separator + "CommitDirectoryTest"
    createDirectory(commitTestDirectory)
    Init.init(commitTestDirectory)

    createFile(commitTestDirectory + File.separator + "test.txt")
    createDirectory(commitTestDirectory + File.separator + "testD")
    createFile(commitTestDirectory + File.separator + "testD" + File.separator + "test1.txt")
  }


  after {
    val commitTestDirectory = new File(".").getCanonicalPath + File.separator + "CommitDirectoryTest"
    val directory = new Directory(new File(commitTestDirectory))
    directory.deleteRecursively()
  }


  "Commit" should "should create a commit if the stage isn't empty and not already committed" in {
    val commitTestDirectory = new File(".").getCanonicalPath + File.separator + "CommitDirectoryTest"

    Add.add(commitTestDirectory, commitTestDirectory, List(commitTestDirectory + File.separator + "test.txt"))
    Commit.commit(commitTestDirectory)

    assert(getFilesDirectory(commitTestDirectory + File.separator + ".sgit" + File.separator + "Commits").length == 1)
  }


  it should "create the correct number of trees in accordance with the stage content" in {
    val commitTestDirectory = new File(".").getCanonicalPath + File.separator + "CommitDirectoryTest"

    Add.add(commitTestDirectory, commitTestDirectory, List(commitTestDirectory + File.separator + "test.txt", commitTestDirectory + File.separator + "testD"))
    Commit.commit(commitTestDirectory)

    assert(getFilesDirectory(commitTestDirectory + File.separator + ".sgit" + File.separator + "Trees").length == 2) //rootTree and testD tree
  }


  it should "update the log file between 2 different commits" in {
    val commitTestDirectory = new File(".").getCanonicalPath + File.separator + "CommitDirectoryTest"

    Add.add(commitTestDirectory, commitTestDirectory, List(commitTestDirectory + File.separator + "test.txt"))
    Commit.commit(commitTestDirectory)
    val contentLog1 = getContentFile(commitTestDirectory + File.separator + ".sgit" + File.separator + "Logs" + File.separator + "master")

    Add.add(commitTestDirectory, commitTestDirectory, List(commitTestDirectory + File.separator + "testD"))
    Commit.commit(commitTestDirectory)
    val contentLog2 = getContentFile(commitTestDirectory + File.separator + ".sgit" + File.separator + "Logs" + File.separator + "master")

    assert(contentLog1 != contentLog2)
  }


  it should "update the branch file between 2 different commits" in {
    val commitTestDirectory = new File(".").getCanonicalPath + File.separator + "CommitDirectoryTest"

    Add.add(commitTestDirectory, commitTestDirectory, List(commitTestDirectory + File.separator + "test.txt"))
    Commit.commit(commitTestDirectory)
    val contentBranch1 = getContentFile(commitTestDirectory + File.separator + ".sgit" + File.separator + "Branches" + File.separator + "master")

    Add.add(commitTestDirectory, commitTestDirectory, List(commitTestDirectory + File.separator + "testD"))
    Commit.commit(commitTestDirectory)
    val contentBranch2 = getContentFile(commitTestDirectory + File.separator + ".sgit" + File.separator + "Branches" + File.separator + "master")

    assert(contentBranch1 != contentBranch2)
  }


  it should "not create a commit if the stage content is the same as the working tree" in {
    val commitTestDirectory = new File(".").getCanonicalPath + File.separator + "CommitDirectoryTest"

    Add.add(commitTestDirectory, commitTestDirectory, List(commitTestDirectory + File.separator + "test.txt"))
    Commit.commit(commitTestDirectory)
    Commit.commit(commitTestDirectory) //we do it twice

    assert(getFilesDirectory(commitTestDirectory + File.separator + ".sgit" + File.separator + "Commits").length == 1)
  }


  it should "create a commit which points towards an empty tree if the stage content is empty" in {
    val commitTestDirectory = new File(".").getCanonicalPath + File.separator + "CommitDirectoryTest"

    val commitHash = Commit.commit(commitTestDirectory)
    val tree = getContentFile(commitTestDirectory + File.separator + ".sgit" + File.separator + "Commits" + File.separator + commitHash).split("\n")(1)
    val contentTree = getContentFile(commitTestDirectory + File.separator + ".sgit" + File.separator + "Trees" + File.separator + tree)

    assert(contentTree.isEmpty)
  }
}
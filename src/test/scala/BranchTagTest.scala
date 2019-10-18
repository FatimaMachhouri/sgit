import java.io.File

import org.scalatest.{BeforeAndAfter, FlatSpec}
import utils.FileIO.{createDirectory, createFile}
import scala.reflect.io.Directory
import command.{Add, BranchTag, Init, Commit}
import utils.Path.getFilesDirectory


class BranchTagTest extends FlatSpec with BeforeAndAfter {

  before {
    val branchTagTestDirectory = new File(".").getCanonicalPath + File.separator + "BranchTagDirectoryTest"
    createDirectory(branchTagTestDirectory)
    Init.init(branchTagTestDirectory)
  }


  after {
    val branchTagTestDirectory = new File(".").getCanonicalPath + File.separator + "BranchTagDirectoryTest"
    val directory = new Directory(new File(branchTagTestDirectory))
    directory.deleteRecursively()
  }


  "BranchTag" should "create a new branch if number of commits != 0" in {
    val branchTagTestDirectory = new File(".").getCanonicalPath + File.separator + "BranchTagDirectoryTest"
    createFile(branchTagTestDirectory + File.separator + "test.txt")
    Add.add(branchTagTestDirectory, branchTagTestDirectory, List(branchTagTestDirectory + File.separator + "test.txt"))
    Commit.commit(branchTagTestDirectory)

    assert(BranchTag.createBranch(branchTagTestDirectory, "branchTest"))
  }


  "it" should "not create a new branch if number of commits == 0" in {
    val branchTagTestDirectory = new File(".").getCanonicalPath + File.separator + "BranchTagDirectoryTest"
    assert(!BranchTag.createBranch(branchTagTestDirectory, "branchTest"))
  }


  it should "create a new tag if number of commits != 0" in {
    val branchTagTestDirectory = new File(".").getCanonicalPath + File.separator + "BranchTagDirectoryTest"
    createFile(branchTagTestDirectory + File.separator + "test.txt")
    Add.add(branchTagTestDirectory, branchTagTestDirectory, List(branchTagTestDirectory + File.separator + "test.txt"))
    Commit.commit(branchTagTestDirectory)

    assert(BranchTag.createTag(branchTagTestDirectory, "tagTest"))
  }


  it should "not create a new tag if number of commits == 0" in {
    val branchTagTestDirectory = new File(".").getCanonicalPath + File.separator + "BranchTagDirectoryTest"
    assert(!BranchTag.createTag(branchTagTestDirectory, "tagTest"))
  }


  it should "list all branches" in {
    val branchTagTestDirectory = new File(".").getCanonicalPath + File.separator + "BranchTagDirectoryTest"
    createFile(branchTagTestDirectory + File.separator + "test.txt")
    Add.add(branchTagTestDirectory, branchTagTestDirectory, List(branchTagTestDirectory + File.separator + "test.txt"))
    Commit.commit(branchTagTestDirectory)

    BranchTag.createBranch(branchTagTestDirectory, "branchTest")

    assert(getFilesDirectory(branchTagTestDirectory + File.separator + ".sgit" + File.separator + "Branches").length.equals(2))
  }


  it should "list all tags" in {
    val branchTagTestDirectory = new File(".").getCanonicalPath + File.separator + "BranchTagDirectoryTest"
    createFile(branchTagTestDirectory + File.separator + "test.txt")
    Add.add(branchTagTestDirectory, branchTagTestDirectory, List(branchTagTestDirectory + File.separator + "test.txt"))
    Commit.commit(branchTagTestDirectory)

    BranchTag.createTag(branchTagTestDirectory, "tagTest")

    assert(getFilesDirectory(branchTagTestDirectory + File.separator + ".sgit" + File.separator + "Tags").length.equals(1))
  }

}
import java.io.File

import org.scalatest.{BeforeAndAfter, FlatSpec}
import command.{Add, Diff, Init}
import utils.FileIO.{createDirectory, createFile, getContentFile, writeInFile}
import utils.Hash.encryptThisString

import scala.reflect.io.Directory
import utils.Path.getFilesDirectory

class DiffTest extends FlatSpec with BeforeAndAfter {

  before {
    val diffTestDirectory = new File(".").getCanonicalPath + File.separator + "DiffDirectoryTest"
    createDirectory(diffTestDirectory)
    createFile(diffTestDirectory + File.separator + "test.txt")
    createFile(diffTestDirectory + File.separator + "test1.txt")
    createFile(diffTestDirectory + File.separator + "test2.txt")
    writeInFile(diffTestDirectory + File.separator + "test.txt", "test content")
    writeInFile(diffTestDirectory + File.separator + "test.txt", "test content 1")
    writeInFile(diffTestDirectory + File.separator + "test.txt", "test content 2")

    Init.init(diffTestDirectory)
  }


  after {
    val diffTestDirectory = new File(".").getCanonicalPath + File.separator + "DiffDirectoryTest"
    val directory = new Directory(new File(diffTestDirectory))
    directory.deleteRecursively()
  }


  "Diff" should "return an empty map if the file content in the working directory is the same as the file content stage" in {
    val diffTestDirectory = new File(".").getCanonicalPath + File.separator + "DiffDirectoryTest"

    Add.add(diffTestDirectory, diffTestDirectory, List(diffTestDirectory + File.separator + "test.txt"))

    assert(Diff.diff(diffTestDirectory).isEmpty)
  }


  it should "return an empty map if the stage and the working tree are empty (no files)" in {
    val diffTestDirectory = new File(".").getCanonicalPath + File.separator + "DiffDirectoryTest"

    val files = List("test.txt" , "test1.txt", "test2.txt")
    files.map(file => new File(diffTestDirectory + File.separator + file).delete())

    assert(Diff.diff(diffTestDirectory).isEmpty)
  }


  it should "return a non-empty map if the files staged are different from the working tree files" in {
    val diffTestDirectory = new File(".").getCanonicalPath + File.separator + "DiffDirectoryTest"

    Add.add(diffTestDirectory, diffTestDirectory, List(diffTestDirectory + File.separator + "test.txt"))
    writeInFile(diffTestDirectory + File.separator + "test.txt", "test content 1")

    assert(!Diff.diff(diffTestDirectory).isEmpty)
  }


  it should "return all files that are different between the stage and the working tree" in {
    val diffTestDirectory = new File(".").getCanonicalPath + File.separator + "DiffDirectoryTest"

    Add.add(diffTestDirectory, diffTestDirectory, List(diffTestDirectory + File.separator + "test.txt"))
    Add.add(diffTestDirectory, diffTestDirectory, List(diffTestDirectory + File.separator + "test1.txt"))
    Add.add(diffTestDirectory, diffTestDirectory, List(diffTestDirectory + File.separator + "test2.txt"))

    writeInFile(diffTestDirectory + File.separator + "test.txt", "test")
    writeInFile(diffTestDirectory + File.separator + "test2.txt", "test2")

    //We modified test.txt and test2.txt
    assert(Diff.diff(diffTestDirectory).keys == Set("test.txt", "test2.txt"))
  }


  it should "return the correct list of differences" in {
    //In this example, the list of differences is List('+c', '-f', '-e')
    val diffTestDirectory = new File(".").getCanonicalPath + File.separator + "DiffDirectoryTest"

    writeInFile(diffTestDirectory + File.separator + "test.txt", "a\nb\nd\nf\ne")
    Add.add(diffTestDirectory, diffTestDirectory, List(diffTestDirectory + File.separator + "test.txt"))
    writeInFile(diffTestDirectory + File.separator + "test.txt", "a\nb\nc\nd")

    val differences = Diff.diff(diffTestDirectory).head._2

    assert(differences == List("+ c", "- f", "- e"))
  }


  it should "return list of files that have been modified" in {
    val diffTestDirectory = new File(".").getCanonicalPath + File.separator + "DiffDirectoryTest"

    Add.add(diffTestDirectory, diffTestDirectory, List(diffTestDirectory + File.separator + "test.txt"))
    Add.add(diffTestDirectory, diffTestDirectory, List(diffTestDirectory + File.separator + "test1.txt"))

    writeInFile(diffTestDirectory + File.separator + "test.txt", "testcontent")

    val stageContent = getContentFile(diffTestDirectory + File.separator + ".sgit" + File.separator + "STAGE")
    val filesInCurrentDirectory = getFilesDirectory(diffTestDirectory)
    val listHashAndFilesDirectory = filesInCurrentDirectory.map(file => List(encryptThisString(getContentFile(file)), file.replace(diffTestDirectory + File.separator, "")))

    assert(Diff.getModifiedFiles(stageContent, listHashAndFilesDirectory).length == 1)
  }


  it should "return an empty list if none of the files have been modified" in {
    val diffTestDirectory = new File(".").getCanonicalPath + File.separator + "DiffDirectoryTest"

    Add.add(diffTestDirectory, diffTestDirectory, List(diffTestDirectory + File.separator + "test.txt"))
    Add.add(diffTestDirectory, diffTestDirectory, List(diffTestDirectory + File.separator + "test1.txt"))
    Add.add(diffTestDirectory, diffTestDirectory, List(diffTestDirectory + File.separator + "test2.txt"))

    val stageContent = getContentFile(diffTestDirectory + File.separator + ".sgit" + File.separator + "STAGE")
    val filesInCurrentDirectory = getFilesDirectory(diffTestDirectory)
    val listHashAndFilesDirectory = filesInCurrentDirectory.map(file => List(encryptThisString(getContentFile(file)), file.replace(diffTestDirectory + File.separator, "")))

    assert(Diff.getModifiedFiles(stageContent, listHashAndFilesDirectory).isEmpty)
  }


  it should "return the list of differences corresponding to 2 texts that are exactly the same" in {
    val matrix = Diff.mostLargestCommonSubSetMatrix(List("a"), List("a"), 0, 0, Map())
    assert(Diff.getDifferences(List("a"), List("a"), 0, 0, matrix, List()).isEmpty)
  }


  it should "return the list of differences corresponding to 2 texts that are totally different" in {
    val matrix = Diff.mostLargestCommonSubSetMatrix(List("a"), List("b"), 0, 0, Map())
    assert(Diff.getDifferences(List("a"), List("b"), 0, 0, matrix, List()) == List("- a", "+ b"))
  }


  it should "return the list of differences corresponding to 2 texts that are partially different" in {
    val matrix = Diff.mostLargestCommonSubSetMatrix(List("a", "b"), List("b"), 0, 0, Map())
    assert(Diff.getDifferences(List("a", "b"), List("b"), 1, 0, matrix, List()) == List("- a"))
  }


  it should "return the matrix corresponding to 2 texts that are exactly the same" in {
    assert(Diff.mostLargestCommonSubSetMatrix(List("a"), List("a"), 0, 0, Map()) == Map((0,0)->1))
  }


  it should "return the matrix corresponding to 2 texts that are totally different" in {
    assert(Diff.mostLargestCommonSubSetMatrix(List("a"), List("b"), 0, 0, Map()) == Map((0,0)->0))
  }


  it should "return the matrix corresponding to 2 texts that are partially different" in {
    assert(Diff.mostLargestCommonSubSetMatrix(List("a", "b"), List("b"), 0, 0, Map()) == Map((0,0)->0, (1,0)->1))
  }
}

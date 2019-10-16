package parser

import java.io.File
import command.{Add, Commit, Diff, Init, Log, Status, BranchTag}
import entities.Repository
import scopt.OParser
import utils.Path
import utils.FileIO.getContentFile

object Parser extends App {

  val builder = OParser.builder[Config]
  val parser1 = {
    import builder._
    OParser.sequence(
      programName("sgit"),
      head("sgit", "1.0"),
      help("help")
        .text("Here : How to use sgit"),
      cmd("init")
        .action((_, c) => c.copy(mode = "init"))
        .text("Creates a .sgit directory in the current directory. If the directory is already a sgit repository, does nothing.")
        .children(
          arg[String]("<path>")
            .optional()
            .action((x, c) => c.copy(path = x))
        ),
      cmd("add")
        .action((_, c) => c.copy(mode = "add"))
        .text("Stages the files.")
        .children(
          arg[String]("<file>...")
            .unbounded()
            .required()
            .action((x, c) => c.copy(files = c.files :+ x))
            .text("File to add to the stage.")
        ),
      cmd("commit")
        .action((_, c) => c.copy(mode = "commit"))
        .text("Commit"),
      cmd("status")
        .action((_, c) => c.copy(mode = "status"))
        .text("Status"),
      cmd("diff")
        .action((_, c) => c.copy(mode = "diff"))
        .text("Diff"),
      cmd("log")
        .action((_, c) => c.copy(mode = "log"))
        .text("Log")
        .children(
          opt[Unit]('p', "patch")
            .text("Log -p")
            .action((_, c) => c.copy(patch = true)),
          opt[Unit]("stat")
            .text("Log --stat")
            .action((_, c) => c.copy(stat = true))
        ),
      cmd("branch")
        .action((_, c) => c.copy(mode = "branch"))
        .text("Branch")
        .children(
          arg[String]("name")
            .optional()
            .action((x, c) => c.copy(name = x))
            .text("Branch name"),
          opt[Unit]('a', "all")
            .optional()
            .action((_, c) => c.copy(displayAll = true))
            .text("Display all branches"),
          opt[Unit]('v', "verbose")
            .optional()
            .action((_, c) => c.copy(verbose = true))
            .text("Show all branches and tags and last commit hash for each one")
        ),
      cmd("tag")
        .action((_, c) => c.copy(mode = "tag"))
        .text("Tag")
        .children(
          arg[String]("name")
            .required()
            .action((x, c) => c.copy(name = x))
            .text("Tag name"),
        ),
    )
  }

  OParser.parse(parser1, args, Config()) match {
    case Some(config) => {

      config.mode match {
        case "init" => {
          val currentPath = new File(".").getCanonicalPath
          val isInitialized = Init.init(currentPath + File.separator + config.path)
          if (isInitialized) println("Initialized an empty sgit repository") else println("You are already in a sgit repository.")
        }

        case "add" => {
          val currentPath = new File(".").getCanonicalPath
          if (Repository.isASgitRepository(currentPath)) {
            val rootPath = Path.sgitParentPath(currentPath)
            Add.add(rootPath, currentPath, config.files)
          }
          else {
            println("You can't run this command, you are not in a sgit repository. Please run sgit init.")
          }
        }

        case "commit" => {
          val currentPath = new File(".").getCanonicalPath
          if (Repository.isASgitRepository(currentPath)) {
            val rootPath = Path.sgitParentPath(currentPath)
            Commit.commit(rootPath)
          }
          else {
            println("You can't run this command, you are not in a sgit repository. Please run sgit init.")
          }
        }

        case "status" => {
          val currentPath = new File(".").getCanonicalPath
          if (Repository.isASgitRepository(currentPath)) {
            val rootPath = Path.sgitParentPath(currentPath)
            println(Status.status(rootPath))
          }
          else {
            println("You can't run this command, you are not in a sgit repository. Please run sgit init.")
          }
        }

        case "diff" => {
          val currentPath = new File(".").getCanonicalPath
          if (Repository.isASgitRepository(currentPath)) {
            val rootPath = Path.sgitParentPath(currentPath)
            val listPathsDifferences = Diff.diff(rootPath)
            println(Diff.prettyFormat(listPathsDifferences, ""))
          }
          else {
            println("You can't run this command, you are not in a sgit repository. Please run sgit init.")
          }
        }

        case "log" => {
          val currentPath = new File(".").getCanonicalPath
          if (Repository.isASgitRepository(currentPath)) {
            val rootPath = Path.sgitParentPath(currentPath)
            val logs = Log.listCommits(rootPath)

            if (!config.patch && !config.stat) { //sgit log
              logs.toList.map(elem => {
                println(elem._1) //branchName
                elem._2.map(commit => println(
                  s"${Console.YELLOW}  commit " + commit.commitHash + Console.RESET + " (" + elem._1 + ")" + "\n" + "  Date:  " + commit.date + "\n")
                )
              })
            }

            else if (config.patch) { //sgit log -p
              logs.toList.map(elem => {
                println(elem._1)

                elem._2.map(commit => {
                  val newFiles = commit.listNewFiles.map(file => "   " + file).mkString("\n")

                  val deletions = commit.listDifferences.toList.map(file => (file._1, file._2.filter(e => e.charAt(0) == '-'))).filter(elem => elem._2.nonEmpty) //for each file, we generate its deletions
                  val additions = commit.listDifferences.toList.map(file => (file._1, file._2.filter(e => e.charAt(0) == '+'))).filter(elem => elem._2.nonEmpty) //same with additions

                  val deletionsRed = deletions.map(file => (file._1, file._2.map(deletion => s"${Console.RED}" + deletion + Console.RESET)))
                  val additionsGreen = additions.map(file => (file._1, file._2.map(addition => s"${Console.GREEN}" + addition + Console.RESET)))

                  val listDiffs = (deletionsRed ++ additionsGreen).groupBy(file => file._1)

                  val differences = listDiffs.toList.map(elem => (elem._1, elem._2.flatMap(e => e._2)))
                  val differencesString = differences.map(file => "   " + file._1 + "\n" + file._2.map(elem => "     " + elem).mkString("\n")).mkString("\n")

                  println(
                    s"${Console.YELLOW}  commit " +
                    commit.commitHash + Console.RESET +
                    " ("+ elem._1 + ")" + "\n" +
                    "  Date:  " + commit.date + "\n" +
                    "  new files :\n" + newFiles + "\n" +
                    "  diff :\n" + differencesString + "\n"
                  )
                })
              })
            }

            else if (config.stat) { //sgit log --stat
              logs.toList.map(f = elem => {
                println(elem._1)

                def plusMinus(list: List[String]): String = {
                  s"${Console.GREEN}" + list.filter(elem => elem.charAt(0) == '+').map(elem => elem.charAt(0)).mkString("") + Console.RESET + s"${Console.RED}" +list.filter(elem => elem.charAt(0) == '-').map(elem => elem.charAt(0)).mkString("") + Console.RESET
                }

                elem._2.map(commit => {
                  val differentFiles = commit.listDifferences.toList.map(elem => "  " + elem._1 + " | " + elem._2.length + " " + plusMinus(elem._2))
                  val newEmptyFiles = commit.listNewFiles.diff(commit.listDifferences.keys.toList)

                  val nbFilesChanged = newEmptyFiles.length + differentFiles.length
                  val nbInsertions = differentFiles.flatten.count(elem => elem == '+')
                  val nbDeletions = differentFiles.flatten.count(elem => elem == '-')

                  println(
                    s"${Console.YELLOW}  commit " +
                      commit.commitHash + Console.RESET +
                      " (" + elem._1 + ")" + "\n" +
                      "  Date:  " + commit.date + "\n" +
                      newEmptyFiles.map(elem => "  " + elem + " | 0").mkString("\n") + "\n" +
                      differentFiles.mkString("\n") + "\n" +
                      "  " + nbFilesChanged + " file(s) changed, " + nbInsertions + " insertion(s)(+), " + nbDeletions + " deletion(s)(-)" + "\n"
                  )
                })
              })
            }

          }
          else {
            println("You can't run this command, you are not in a sgit repository. Please run sgit init.")
          }
        }

        case "branch" => {
          val currentPath = new File(".").getCanonicalPath
          if (Repository.isASgitRepository(currentPath)) {
            val rootPath = Path.sgitParentPath(currentPath)
            if (!config.name.isEmpty) BranchTag.createBranch(rootPath, config.name)

            else if (config.verbose) {
              val branches = BranchTag.listBranches(rootPath).map(b =>
                if (b.split(" ")(0) == getContentFile(rootPath + File.separator + ".sgit" + File.separator + "HEAD")) s"${Console.GREEN}* Branch : " + b + Console.RESET
                else "  Branch : " + b
              ).mkString("\n")
              val tags = BranchTag.listTags(rootPath).map(t => "  Tag    : " + t).mkString("\n")
              println(branches + "\n" + tags)
            }

            else {
              val branches = {
                val listBranches = BranchTag.listBranches(rootPath)
                if (listBranches.isEmpty) ""
                else listBranches.map(b =>
                  if (b.split(" ")(0) == getContentFile(rootPath + File.separator + ".sgit" + File.separator + "HEAD")) s"${Console.GREEN}* Branch : " + b.split(" ")(0) + Console.RESET
                  else "  Branch : " + b.split(" ")(0)
                ).mkString("\n")
              }
              val tags = {
                val listTags = BranchTag.listTags(rootPath)
                if (listTags.isEmpty) ""
                else listTags.map(t => "  Tag    : " + t.split(" ")(0)).mkString("\n")
              }
              println(branches + "\n" + tags)
            }
          }
          else {
            println("You can't run this command, you are not in a sgit repository. Please run sgit init.")
          }
        }

        case "tag" => {
          val currentPath = new File(".").getCanonicalPath
          if (Repository.isASgitRepository(currentPath)) {
            val rootPath = Path.sgitParentPath(currentPath)
            BranchTag.createTag(rootPath, config.name)
          }
          else {
            println("You can't run this command, you are not in a sgit repository. Please run sgit init.")
          }
        }

        case _ => {
          "Not a sgit command"
        }
      }
    }

    case _ =>
  }

}
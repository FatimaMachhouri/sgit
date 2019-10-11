package parser

import java.io.File

import command.{Add, Commit, Init}
import entities.Repository
import scopt.OParser
import utils.CurrentPath

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
        .text("Creates a .sgit directory in the current directory."),
      cmd("add")
        .action((_, c) => c.copy(mode = "add"))
        .text("Stages the files.")
        .children(
          arg[File]("<file>...")
            .unbounded()
            .required()
            .action((x, c) => c.copy(files = c.files :+ x))
            .text("File to add to the stage.")
        ),
      cmd("commit")
        .action((_, c) => c.copy(mode = "commit"))
        .text("Commit")
    )
  }

  OParser.parse(parser1, args, Config()) match {
    case Some(config) => {

      config.mode match {
        case "init" => {
          Init.init()
        }

        case "add" => {
          if (Repository.isASgitRepository()) {
            val currentPath = CurrentPath.sgitParentPath()
            Add.add(args.tail.toList)
          }
          else {
            println("You can't run this command, you are not in a sgit repository. Please run sgit init.")
          }
        }

        case "commit" => {
          if (Repository.isASgitRepository()) {
            val currentPath = CurrentPath.sgitParentPath()
            Commit.commit()
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
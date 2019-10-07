package parser

import command.Init
import scopt.OParser

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
        .text(
          "Creates a .sgit directory in the current directory"
        )
        .children(
          arg[String]("<path>")
            .optional()
            .action((x, c) => c.copy(path = x))
        )
    )
  }

  OParser.parse(parser1, args, Config()) match {
    case Some(config) => {

      config.mode match {
        case "init" => {
          val i = new Init()
          i.init()
        }
        case _ => {
          "Other"
        }
      }
    }

    case _ =>
  }

}
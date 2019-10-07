package parser

import java.io.File
case class Config(
                   mode: String = "",
                   path: String = "",
                   files: Seq[File] = Seq()
                 )
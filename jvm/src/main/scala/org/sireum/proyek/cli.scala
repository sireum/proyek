// #Sireum
/*
 Copyright (c) 2021, Robby, Kansas State University
 All rights reserved.

 Redistribution and use in source and binary forms, with or without
 modification, are permitted provided that the following conditions are met:

 1. Redistributions of source code must retain the above copyright notice, this
    list of conditions and the following disclaimer.
 2. Redistributions in binary form must reproduce the above copyright notice,
    this list of conditions and the following disclaimer in the documentation
    and/or other materials provided with the distribution.

 THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS IS" AND
 ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED
 WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE
 DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT OWNER OR CONTRIBUTORS BE LIABLE FOR
 ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES
 (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES;
 LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND
 ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT
 (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS
 SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
 */

package org.sireum.proyek

import org.sireum._
import org.sireum.cli.CliOpt._

object cli {

  val ivyOptGroup: OptGroup = OptGroup(
    name = "Ivy Dependencies",
    opts = ISZ(
      Opt(name = "cache", longKey = "cache", shortKey = Some('c'),
        tpe = Type.Path(F, None()),
        description = "Ivy cache directory (defaults to couriser's default cache directory)"
      ),
      Opt(name = "sources", longKey = "no-sources", shortKey = None(),
        tpe = Type.Flag(T),
        description = "Disable retrieval of source files from Ivy dependencies"
      ),
      Opt(name = "docs", longKey = "no-docs", shortKey = None(),
        tpe = Type.Flag(T),
        description = "Disable retrieval of javadoc files from Ivy dependencies"
      ),
      Opt(name = "repositories", longKey = "repositories", shortKey = Some('r'),
        tpe = Type.Str(sep = Some(','), default = None()),
        description = "Disable retrieval of javadoc files from Ivy dependencies"
      )
    )
  )

  val projectOptGroup: OptGroup = OptGroup(
    name = "Project",
    opts = ISZ(
      Opt(name = "json", longKey = "json", shortKey = None(),
        tpe = Type.Path(F, None()),
        description = "The JSON file to load project definitions from (mutually exclusive with the 'project' option)"
      ),
      Opt(name = "name", longKey = "name", shortKey = Some('n'),
        tpe = Type.Str(sep = None(), default = None()),
        description = "Project name (defaults to the directory name of <dir>)"
      ),
      Opt(name = "outputDirName", longKey = "out", shortKey = Some('o'),
        tpe = Type.Str(sep = None(), default = Some("out")),
        description = "Output directory name under <dir>"
      ),
      Opt(name = "project", longKey = "project", shortKey = None(),
        tpe = Type.Path(F, None()),
        description = "The project.cmd file accepting the 'json' argument (defaults to <dir>${Os.fileSep}bin${Os.fileSep}project.cmd; mutually exclusive with the 'json' option)"
      ),
      Opt(name = "symlink", longKey = "symlink", shortKey = None(),
        tpe = Type.Flag(F),
        description = "Follow symbolic link when searching for files"
      ),
      Opt(name = "versions", longKey = "versions", shortKey = Some('v'),
        tpe = Type.Path(F, None()),
        description = "The properties file containing version information (defaults to <dir>${Os.fileSep}versions.properties)"
      )
    )
  )

  val incrementalOptGroup: OptGroup = OptGroup(
    name = "Incremental Compilation",
    opts = ISZ(
      Opt(name = "fresh", longKey = "fresh", shortKey = Some('f'),
        tpe = Type.Flag(F),
        description = "Fresh compilation from a clean slate"
      ),
      Opt(name = "sha3", longKey = "sha3", shortKey = None(),
        tpe = Type.Flag(F),
        description = "Use SHA3 instead of time stamp for detecting file changes"
      )
    )
  )

  val assembleTool: Tool = Tool(
    name = "assemble",
    command = "assemble",
    description = "Sireum Proyek Jar Assembler",
    header = "Sireum Proyek Jar Assembler",
    usage = "<options>* <dir>",
    opts = ISZ(
      Opt(name = "mainClass", longKey = "main", shortKey = Some('m'),
        tpe = Type.Str(None(), None()),
        description = "The main class fully qualified name"
      ),
      Opt(name = "par", longKey = "par", shortKey = Some('p'),
        tpe = Type.Flag(F),
        description = "Enable parallelization"
      )
    ),
    groups = ISZ(
      projectOptGroup,
      incrementalOptGroup,
      ivyOptGroup
    )
  )

  val compileTool: Tool = Tool(
    name = "compile",
    command = "compile",
    description = "Sireum Proyek Compiler",
    header = "Sireum Proyek Compiler",
    usage = "<options>* <dir>",
    opts = ISZ(
      Opt(name = "par", longKey = "par", shortKey = Some('p'),
        tpe = Type.Flag(F),
        description = "Enable parallelization"
      )
    ),
    groups = ISZ(
      projectOptGroup,
      incrementalOptGroup,
      ivyOptGroup
    )
  )

  val iveTool: Tool = Tool(
    name = "ive",
    command = "ive",
    description = "Sireum IVE Proyek Generator",
    header = "Sireum IVE Proyek",
    usage = "<options>* <dir>",
    opts = ISZ(
      Opt(name = "force", longKey = "force", shortKey = Some('f'),
        tpe = Type.Flag(F),
        description = "Force generation of application-wide configurations (e.g., JDK info, etc.)"
      )
    ),
    groups = ISZ(
      projectOptGroup,
      ivyOptGroup
    )
  )

  val group: Group = Group(
    name = "proyek",
    description = "Project tools",
    header = "Sireum Proyek",
    unlisted = F,
    subs = ISZ(assembleTool, compileTool, iveTool)
  )

}

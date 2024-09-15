// #Sireum
/*
 Copyright (c) 2017-2024, Robby, Kansas State University
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
        description = "Ivy cache directory (defaults to Coursier's default cache directory)"
      ),
      Opt(name = "docs", longKey = "no-docs", shortKey = None(),
        tpe = Type.Flag(T),
        description = "Disable retrieval of javadoc files from Ivy dependencies"
      ),
      Opt(name = "sources", longKey = "no-sources", shortKey = None(),
        tpe = Type.Flag(T),
        description = "Disable retrieval of source files from Ivy dependencies"
      ),
      Opt(name = "repositories", longKey = "repositories", shortKey = Some('r'),
        tpe = Type.Str(sep = Some(','), default = None()),
        description = "Additional repository URLs to retrieve Ivy dependencies from"
      )
    )
  )

  val projectOptGroup: OptGroup = OptGroup(
    name = "Project",
    opts = ISZ(
      Opt(name = "ignoreRuntime", longKey = "ignore-runtime", shortKey = None(),
        tpe = Type.Flag(F),
        description = "Ignore runtime library dependency version when detecting changes"
      ),
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
        description = "The project.cmd file accepting the 'json' argument (defaults to <dir>${Os.fileSep}bin${Os.fileSep}project-standalone.cmd, or <dir>${Os.fileSep}bin${Os.fileSep}project.cmd; mutually exclusive with the 'json' option)"
      ),
      Opt(name = "slice", longKey = "slice", shortKey = None(),
        tpe = Type.Str(Some(','), None()),
        description = "Slice the project starting from the given module IDs and their dependencies"
      ),
      Opt(name = "symlink", longKey = "symlink", shortKey = None(),
        tpe = Type.Flag(F),
        description = "Follow symbolic link when searching for files"
      ),
      Opt(name = "versions", longKey = "versions", shortKey = Some('v'),
        tpe = Type.Path(T, None()),
        description = "The properties file(s) containing version information (defaults to <dir>${Os.fileSep}versions.properties)"
      )
    )
  )

  val commonCompileOpts: ISZ[Opt] = ISZ(
    Opt(name = "javac", longKey = "javac", shortKey = None(),
      tpe = Type.Str(Some(','), Some("-source, 17, -target, 17, -encoding, utf8, -XDignore.symbol.file, -Xlint:-options, -Xlint:deprecation, -proc:none")),
      description = "Javac options"
    ),
    Opt(name = "fresh", longKey = "fresh", shortKey = Some('f'),
      tpe = Type.Flag(F),
      description = "Fresh compilation from a clean slate"
    ),
    org.sireum.logika.cli.parOpt,
    Opt(name = "recompile", longKey = "recompile", shortKey = None(),
      tpe = Type.Str(Some(','), None()),
      description = "Module IDs to force recompilation on"
    ),
    Opt(name = "scalac", longKey = "scalac", shortKey = None(),
      tpe = Type.Str(Some(','), Some("-release, 17, -deprecation, -Yrangepos, -Ydelambdafy:method, -feature, -unchecked, -Xfatal-warnings, -language:postfixOps, -Wconf:cat=scala3-migration&msg=legacy-binding^msg=symbol^msg=outer:s")),
      description = "Scalac options"
    ),
    Opt(name = "sha3", longKey = "sha3", shortKey = None(),
      tpe = Type.Flag(F),
      description = "Use SHA3 instead of time stamp for detecting file changes"
    )
  )

  val compileOptGroup: OptGroup = OptGroup(
    name = "Compilation",
    opts = commonCompileOpts :+
      Opt(name = "skipCompile", longKey = "skip-compile", shortKey = None(),
        tpe = Type.Flag(F),
        description = "Skip compilation"
      )
  )

  val assembleTool: Tool = Tool(
    name = "assemble",
    command = "assemble",
    description = "Proyek jar assembler",
    header = "Sireum Proyek Jar Assembler",
    usage = "<options>* <dir>",
    usageDescOpt = None(),
    opts = ISZ(
      Opt(name = "includeSources", longKey = "include-sources", shortKey = None(),
        tpe = Type.Flag(F),
        description = "Include source files"
      ),
      Opt(name = "includeTests", longKey = "include-tests", shortKey = None(),
        tpe = Type.Flag(F),
        description = "Include test classes"
      ),
      Opt(name = "jar", longKey = "jar", shortKey = Some('j'),
        tpe = Type.Str(None(), None()),
        description = "The assembled jar filename (defaults to the project name)"
      ),
      Opt(name = "noDeps", longKey = "no-deps", shortKey = None(),
        tpe = Type.Flag(F),
        description = "Exclude library dependencies"
      ),
      Opt(name = "mainClass", longKey = "main", shortKey = Some('m'),
        tpe = Type.Str(None(), None()),
        description = "The main class fully qualified name"
      ),
      Opt(name = "meta", longKey = "meta", shortKey = None(),
        tpe = Type.Flag(F),
        description = "Generate Scalameta semanticdb"
      ),
      Opt(name = "isNative", longKey = "native", shortKey = None(),
        tpe = Type.Flag(F),
        description = "Generates native image"
      ),
      Opt(name = "uber", longKey = "uber", shortKey = None(),
        tpe = Type.Flag(F),
        description = "Generates uber jar"
      )
    ),
    groups = ISZ(
      projectOptGroup,
      compileOptGroup,
      ivyOptGroup
    )
  )

  val compileTool: Tool = Tool(
    name = "compile",
    command = "compile",
    description = "Proyek compiler",
    header = "Sireum Proyek Compiler",
    usage = "<options>* <dir>",
    usageDescOpt = None(),
    opts = commonCompileOpts :+
      Opt(name = "js", longKey = "js", shortKey = None(),
        tpe = Type.Flag(F),
        description = "Compile using Scala.js"
      ) :+
      Opt(name = "meta", longKey = "meta", shortKey = None(),
        tpe = Type.Flag(F),
        description = "Generate Scalameta semanticdb"
      ),
    groups = ISZ(
      projectOptGroup,
      ivyOptGroup
    )
  )

  val depTool: Tool = Tool(
    name = "dep",
    command = "dep",
    description = "Sireum proyek Ivy dependency visualizer",
    header = "Sireum Proyek Ivy Dependency Visualizer",
    usage = "<options>* <dir>",
    usageDescOpt = None(),
    opts = ISZ(
      Opt(name = "js", longKey = "js", shortKey = None(),
        tpe = Type.Flag(F),
        description = "Scala.js dependency"
      )
    ),
    groups = ISZ(
      projectOptGroup,
      ivyOptGroup
    )
  )

  val exportTool: Tool = Tool(
    name = "export",
    command = "export",
    description = "Sireum proyek exporter",
    header = "Sireum Proyek Exporter",
    usage = "<options>* <dir>",
    usageDescOpt = None(),
    opts = commonCompileOpts ++ ISZ(
      Opt(name = "target", longKey = "target", shortKey = None(),
        tpe = Type.Choice(name = "buildTool", sep = Some(','), elements = ISZ("bloop", "mill")),
        description = "Build tool target"
      )
    ),
    groups = ISZ(
      projectOptGroup
    )
  )


  val iveTool: Tool = Tool(
    name = "ive",
    command = "ive",
    description = "Sireum IVE proyek generator",
    header = "Sireum IVE Proyek Generator",
    usage = "<options>* <dir>",
    usageDescOpt = None(),
    opts = ISZ(
      Opt(name = "empty", longKey = "empty", shortKey = None(),
        tpe = Type.Flag(F),
        description = "Create an empty project definition"
      ),
      Opt(name = "edition", longKey = "edition", shortKey = Some('e'),
        tpe = Type.Choice("edition", None(), ISZ("community", "ultimate", "server")),
        description = "IntelliJ edition (auto-detected if there is only one installed)"
      ),
      Opt(name = "force", longKey = "force", shortKey = Some('f'),
        tpe = Type.Flag(F),
        description = "Force generation of application-wide configurations (e.g., JDK info, etc.)"
      ),
      Opt(name = "rebuildIve", longKey = "rebuild-ive", shortKey = None(),
        tpe = Type.Flag(F),
        description = "Rebuild IVE"
      ),
    ) ++ (for (opt <- commonCompileOpts if opt.name == "javac" || opt.name == "scalac") yield opt),
    groups = ISZ(
      projectOptGroup,
      ivyOptGroup
    )
  )


  val slangCheckProyekTool: Tool = Tool(
    name = "slangcheck",
    command = "slangcheck",
    description = "Slang Check generator",
    header = "Slang Check generator",
    usage = "<option>* <dir> <slang-file>+",
    usageDescOpt = None(),
    opts = ISZ(
      Opt(name = "license", longKey = "license", shortKey = Some('l'),
        tpe = Type.Path(multiple = F, default = None()), description = "License file to be inserted in the file header"),
      Opt(name = "packageName", longKey = "packageName", shortKey = Some('p'),
        tpe = Type.Path(multiple = F, default = None()), description = "Package name for generators"),
      Opt(name = "outputDir", longKey = "output-dir", shortKey = Some('o'),
        tpe = Type.Path(multiple = F, default = Some(".")), description = "Output directory for the generated Slang Check files"),
      Opt(name = "testDir", longKey = "test-dir", shortKey = Some('t'),
        tpe = Type.Path(multiple = F, default = None()), description = "Output directory for the generated unit tests"),

      org.sireum.logika.cli.parOpt,
      org.sireum.lang.cli.strictAliasingOpt,
      org.sireum.lang.cli.verboseOpt
    ),
    groups = ISZ(
      org.sireum.proyek.cli.projectOptGroup,
      org.sireum.proyek.cli.ivyOptGroup
    )
  )


  val logikaProyekTool: Tool = Tool(
    name = "logika",
    command = "logika",
    description = "Sireum Logika for Proyek",
    header = "Sireum Logika for Proyek",
    usage = "<options>* <dir> <file>*",
    usageDescOpt = None(),
    opts = ISZ(
      Opt(
        name = "all", longKey = "all", shortKey = None(),
        tpe = Type.Flag(F),
        description = "Check all Slang files"
      ),
      org.sireum.lang.cli.strictAliasingOpt,
      org.sireum.lang.cli.verboseOpt
    ),
    groups = ISZ(
      projectOptGroup,
      ivyOptGroup
    ) ++ org.sireum.logika.cli.logikaVerifier.groups
  )


  val publishTool: Tool = Tool(
    name = "publish",
    command = "publish",
    description = "Proyek publisher",
    header = "Sireum Proyek Publisher",
    usage = "<options>* <dir> <org.name>",
    usageDescOpt = None(),
    opts = ISZ(
      Opt(
        name = "m2", longKey = "m2", shortKey = None(),
        tpe = Type.Path(F, None()),
        description = """Local m2 repository (defaults to the user home's .m2 directory)"""
      ),
      Opt(
        name = "target", longKey = "target", shortKey = None(),
        tpe = Type.Choice(name = "target", sep = Some(','), elements = ISZ("all", "jvm", "js")),
        description = """Publication target"""
      ),
      Opt(
        name = "version", longKey = "version", shortKey = None(),
        tpe = Type.Str(None(), None()),
        description = """Publication version (defaults to using git commit date, time, and abbreviated hash)"""
      )
    ),
    groups = ISZ(
      projectOptGroup,
      compileOptGroup,
      ivyOptGroup
    )
  )


  val runTool: Tool = Tool(
    name = "run",
    command = "run",
    description = "Proyek program runner",
    header = "Sireum Proyek Program Runner",
    usage = "<options>* <dir> <class-name> <arg>*",
    usageDescOpt = None(),
    opts = ISZ(
      Opt(
        name = "dir", longKey = "dir", shortKey = Some('d'),
        tpe = Type.Path(F, None()),
        description = "Working directory (defaults to current working directory)"
      ),
      Opt(name = "java", longKey = "java", shortKey = None(),
        tpe = Type.Str(Some(','), None()),
        description = "Java options"
      )
    ),
    groups = ISZ(
      projectOptGroup,
      compileOptGroup,
      ivyOptGroup
    )
  )


  val statsTool: Tool = Tool(
    name = "stats",
    command = "stats",
    description = "Proyek statistics reporter",
    header = "Sireum Proyek Statistics Reporter",
    usage = "<options>* <dir> [ <file.csv> ]",
    usageDescOpt = None(),
    opts = ISZ(),
    groups = ISZ(
      projectOptGroup,
      ivyOptGroup
    )
  )


  val testTool: Tool = Tool(
    name = "test",
    command = "test",
    description = "Proyek test runner",
    header = "Sireum Proyek Test Runner",
    usage = "<options>* <dir> <root-package-name>*",
    usageDescOpt = None(),
    opts = ISZ(
      Opt(
        name = "classes", longKey = "classes", shortKey = None(),
        tpe = Type.Str(Some(','), None()),
        description = "Specific fully-qualified test class names to run"
      ),
      Opt(name = "coverage", longKey = "coverage", shortKey = None(),
        tpe = Type.Path(F, None()),
        description = "JaCoCo exec, classdumpdir, report path prefix (without .exec, .dump, .coverage)"),
      Opt(name = "java", longKey = "java", shortKey = None(),
        tpe = Type.Str(Some(','), None()),
        description = "Java options"
      ),
      Opt(
        name = "packages", longKey = "packages", shortKey = None(),
        tpe = Type.Str(Some(','), None()),
        description = "Specific fully-qualified test package names to run"
      ),
      Opt(
        name = "suffixes", longKey = "suffixes", shortKey = None(),
        tpe = Type.Str(Some(','), None()),
        description = "Specific test class name suffixes to run"
      )
    ),
    groups = ISZ(
      projectOptGroup,
      compileOptGroup,
      ivyOptGroup
    )
  )


  val tipeProyekTool: Tool = Tool(
    name = "tipe",
    command = "tipe",
    description = "Slang proyek type checker",
    header = "Sireum Proyek Type Checker",
    usage = "<options>* <dir>",
    usageDescOpt = None(),
    opts = ISZ(
      org.sireum.logika.cli.parOpt,
      org.sireum.lang.cli.strictAliasingOpt,
      org.sireum.lang.cli.verboseOpt
    ),
    groups = ISZ(
      projectOptGroup,
      ivyOptGroup
    )
  )


  val group: Group = Group(
    name = "proyek",
    description = "Build tools",
    header = "Sireum Proyek: Build Tools for Slang Projects",
    unlisted = F,
    subs = ISZ(assembleTool, compileTool, depTool, exportTool, iveTool, logikaProyekTool, publishTool, runTool,
      slangCheckProyekTool, statsTool, testTool, tipeProyekTool)
  )

}

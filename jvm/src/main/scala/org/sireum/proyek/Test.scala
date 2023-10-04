// #Sireum
/*
 Copyright (c) 2017-2023, Robby, Kansas State University
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
import org.sireum.project._
import org.sireum.proyek.Proyek._

object Test {

  val EXEC_MISSING: Z = -5
  val DUMP_MISSING: Z = -6

  def run(path: Os.Path,
          outDirName: String,
          project: Project,
          projectName: String,
          dm: DependencyManager,
          javaOptions: ISZ[String],
          classNames: ISZ[String],
          suffixes: ISZ[String],
          packageNames: ISZ[String],
          names: ISZ[String],
          coverageOpt: Option[String]): Z = {

    val proyekDir = getProyekDir(path, outDirName, projectName, F)
    val projectOutDir = proyekDir / "modules"

    var testClasspath = ISZ[String]()

    var classpath = HashSSet.empty[String] ++ (for (
      cif <- dm.fetch(ISZ(s"${DependencyManager.scalaTestKey}${dm.scalaTestVersion}"))
    ) yield cif.path.string)

    for (m <- project.modules.values) {
      val mDir = projectOutDir / m.id / mainOutDirName
      if (mDir.exists) {
        classpath = classpath + mDir.string
      }
      classpath = classpath ++ (for (r <- ProjectUtil.moduleResources(m) ++ ProjectUtil.moduleTestResources(m)) yield r.string)
      classpath = classpath ++ (for (lib <- dm.fetchDiffLibs(m)) yield Os.path(lib.main).string)
      val mTestDir = projectOutDir / m.id / testOutDirName
      if (mTestDir.exists) {
        classpath = classpath + mTestDir.string
        testClasspath = testClasspath :+ mTestDir.string
      }
    }

    val scalaLib = (dm.scalaHome / "lib" / "scala-library.jar").string
    var args = javaOptions ++ ISZ[String](
      "-ea", "-classpath", st"${(scalaLib +: classpath.elements, Os.pathSep)}".render)

    val jacocoCli = dm.sireumHome / "lib" / "jacococli.jar"
    coverageOpt match {
      case Some(p) =>
        val prefix = Os.path(p)
        val exec = (prefix.up / s"${prefix.name}.exec").canon
        val dump = (prefix.up / s"${prefix.name}.dump").canon
        exec.removeAll()
        dump.removeAll()
        dump.mkdirAll()
        val jacocoAgent = dm.sireumHome / "lib" / "jacocoagent.jar"
        args = args :+ s"-javaagent:$jacocoAgent=destfile=$exec,classdumpdir=$dump"
      case _ =>
    }

    args = args ++ ISZ[String]("org.scalatest.tools.Runner",
      "-oF", "-P1",
      "-R",
      st""""${
        (if (Os.isWin) for (p <- testClasspath) yield ops.StringOps(p).replaceAllLiterally("\\", "\\\\")
        else testClasspath, " ")
      }"""".render
    )
    args = args ++ (for (args2 <- for (name <- classNames) yield
      ISZ[String]("-s", ops.StringOps(name).trim); arg <- args2) yield arg)
    args = args ++ (for (args2 <- for (suffix <- suffixes) yield
      ISZ[String]("-q", ops.StringOps(suffix).trim); arg <- args2) yield arg)
    args = args ++ (for (args2 <- for (name <- packageNames) yield
      ISZ[String]("-m", ops.StringOps(name).trim); arg <- args2) yield arg)
    args = args ++ (for (args2 <- for (name <- names) yield ISZ[String]("-w", name); arg <- args2) yield arg)

    val argFile = proyekDir / "java-test-args"
    argFile.writeOver(st"${(args, "\n")}".render)

    val javaExe = dm.javaHome / "bin" / (if (Os.isWin) "java.exe" else "java")
    var exitCode = proc"$javaExe @$argFile".at(path).console.run().exitCode

    if (exitCode == 0) {
      coverageOpt match {
        case Some(p) =>
          val prefix = Os.path(p)
          val exec = (prefix.up / s"${prefix.name}.exec").canon
          val dump = (prefix.up / s"${prefix.name}.dump").canon

          if (!exec.exists) {
            eprintln(s"$exec was not generated")
            return EXEC_MISSING
          }
          if (dump.list.isEmpty) {
            eprintln(s"$dump was not generated")
            return DUMP_MISSING
          }
          val csv = (prefix.up / s"${prefix.name}.coverage.csv").canon
          val html = (prefix.up / s"${prefix.name}.coverage").canon
          csv.removeAll()
          html.removeAll()
          println("Generating coverage report ...")
          println(s"* $csv")
          println(s"* $html")
          var commands = ISZ[String]("-jar", jacocoCli.string, "report", exec.string, "--encoding",
            "UTF-8", "--classfiles", dump.string, "--csv", csv.string, "--html", html.string)
          for (m <- project.modules.values; src <- ProjectUtil.moduleSources(m) ++ ProjectUtil.moduleTestSources(m)) {
            commands = commands ++ ISZ[String]("--sourcefiles", src.string)
          }

          val jacocoArgFile = proyekDir / "jacoco-args"
          jacocoArgFile.writeOver(st"${(commands, "\n")}".render)

          exitCode = proc"$javaExe @$jacocoArgFile".at(path).run().exitCode
          println()
        case _ =>
      }
    }

    return exitCode
  }

}
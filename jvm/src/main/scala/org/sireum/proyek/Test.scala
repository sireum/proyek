// #Sireum
/*
 Copyright (c) 2017-2026,Robby, Kansas State University
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

  def argFileValue(value: String): String = {
    return encodeArgFileValue(value, F)
  }

  def encodeArgFileValue(value: String, forceQuote: B): String = {
    var quote = forceQuote || value == ""
    var escaped = ISZ[C]()
    for (c <- conversions.String.toCis(value)) {
      c match {
        case '\\' =>
          quote = T
          escaped = escaped :+ '\\' :+ '\\'
        case '\"' =>
          quote = T
          escaped = escaped :+ '\\' :+ '\"'
        case '\r' =>
          quote = T
          escaped = escaped :+ '\\' :+ 'r'
        case '\n' =>
          quote = T
          escaped = escaped :+ '\\' :+ 'n'
        case ' ' =>
          quote = T
          escaped = escaped :+ c
        case '\t' =>
          quote = T
          escaped = escaped :+ c
        case '\f' =>
          quote = T
          escaped = escaped :+ c
        case '\'' =>
          quote = T
          escaped = escaped :+ c
        case '#' =>
          quote = T
          escaped = escaped :+ c
        case _ =>
          escaped = escaped :+ c
      }
    }
    if (!quote) {
      return value
    }
    return st"\"${conversions.String.fromCis(escaped)}\"".render
  }

  def argFileContent(args: ISZ[(String, B)]): String = {
    return st"${(for (arg <- args) yield encodeArgFileValue(arg._1, arg._2), "\n")}".render
  }

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
          tests: ISZ[String],
          coverageOpt: Option[String],
          isJUnit5: B,
          parTest: B): Z = {

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

    val jacocoCli = dm.sireumHome / "lib" / "jacococli.jar"
    var jacocoArgs = ISZ[String]()
    coverageOpt match {
      case Some(p) =>
        val prefix = Os.path(p)
        val exec = (prefix.up / s"${prefix.name}.exec").canon
        val dump = (prefix.up / s"${prefix.name}.dump").canon
        exec.removeAll()
        dump.removeAll()
        dump.mkdirAll()
        val jacocoAgent = dm.sireumHome / "lib" / "jacocoagent.jar"
        jacocoArgs = jacocoArgs :+ s"-javaagent:$jacocoAgent=destfile=$exec,classdumpdir=$dump"
      case _ =>
    }

    var args = javaOptions ++ jacocoArgs ++ ISZ[String](
      "--sun-misc-unsafe-memory-access=allow", "--enable-native-access=ALL-UNNAMED",
      "-ea", "-classpath", st"${(scalaLib +: classpath.elements, Os.pathSep)}".render)

    var exitCode: Z = 0
    val javaExe = dm.javaHome / "bin" / (if (Os.isWin) "java.exe" else "java")

    if (isJUnit5) {
      // JUnit Platform: discover and run tests via registered TestEngine implementations
      if (testClasspath.nonEmpty) {
        // Fetch JUnit Platform launcher JARs from DependencyManager
        val junitLauncherJars: ISZ[String] = for (
          cif <- dm.fetch(ISZ(s"${DependencyManager.junitPlatformLauncherKey}${dm.junitPlatformLauncherVersion}"))
        ) yield cif.path.string

        // Extract JUnit5Runner from sireum.jar to avoid full sireum.jar on classpath
        val junit5RunnerDir = proyekDir / "junit5-runner"
        junit5RunnerDir.removeAll()
        junit5RunnerDir.mkdirAll()
        val sireumJar = dm.sireumHome / "bin" / "sireum.jar"
        val jarExe = dm.javaHome / "bin" / "jar"
        proc"$jarExe xf $sireumJar org/sireum/proyek/JUnit5Runner.class".at(junit5RunnerDir).run()

        val junit5Classpath = (scalaLib +: (classpath.elements ++ junitLauncherJars)) :+ junit5RunnerDir.string
        var junit5Args = javaOptions ++ jacocoArgs ++ ISZ[String](
          "--sun-misc-unsafe-memory-access=allow", "--enable-native-access=ALL-UNNAMED",
          "-ea", "-classpath", st"${(junit5Classpath, Os.pathSep)}".render,
          "org.sireum.proyek.JUnit5Runner"
        )
        junit5Args = junit5Args ++ (for (args2 <- for (name <- classNames) yield
          ISZ[String]("-s", ops.StringOps(name).trim); arg <- args2) yield arg)
        junit5Args = junit5Args ++ (for (args2 <- for (suffix <- suffixes) yield
          ISZ[String]("-q", ops.StringOps(suffix).trim); arg <- args2) yield arg)
        junit5Args = junit5Args ++ (for (args2 <- for (name <- packageNames) yield
          ISZ[String]("-m", ops.StringOps(name).trim); arg <- args2) yield arg)
        junit5Args = junit5Args ++ (for (args2 <- for (name <- names) yield
          ISZ[String]("-w", name); arg <- args2) yield arg)
        junit5Args = junit5Args ++ testClasspath
        val junit5ArgFile = proyekDir / "java-junit5-test-args"
        junit5ArgFile.writeOver(argFileContent(for (arg <- junit5Args) yield (arg, F)))
        exitCode = proc"$javaExe @$junit5ArgFile".at(path).console.run().exitCode
      }
    } else {
      // ScalaTest Runner
      var scalaTestArgs: ISZ[(String, B)] = for (arg <- args) yield (arg, F)
      scalaTestArgs = scalaTestArgs ++ ISZ[(String, B)](
        ("org.scalatest.tools.Runner", F),
        ("-oF", F),
        (if (parTest) s"-P${Os.numOfProcessors}" else "-P1", F),
        ("-R", F),
        (st"${(testClasspath, " ")}".render, T)
      )
      for (name <- classNames) {
        scalaTestArgs = scalaTestArgs ++ ISZ[(String, B)](("-s", F), (ops.StringOps(name).trim, F))
      }
      for (suffix <- suffixes) {
        scalaTestArgs = scalaTestArgs ++ ISZ[(String, B)](("-q", F), (ops.StringOps(suffix).trim, F))
      }
      for (name <- packageNames) {
        scalaTestArgs = scalaTestArgs ++ ISZ[(String, B)](("-m", F), (ops.StringOps(name).trim, F))
      }
      for (name <- names) {
        scalaTestArgs = scalaTestArgs ++ ISZ[(String, B)](("-w", F), (name, F))
      }
      for (test <- tests) {
        scalaTestArgs = scalaTestArgs ++ ISZ[(String, B)](("-z", F), (ops.StringOps(test).trim, T))
      }

      val argFile = proyekDir / "java-test-args"
      argFile.writeOver(argFileContent(scalaTestArgs))

      exitCode = proc"$javaExe @$argFile".at(path).console.run().exitCode
    }

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
          jacocoArgFile.writeOver(argFileContent(for (arg <- commands) yield (arg, F)))

          exitCode = proc"$javaExe @$jacocoArgFile".at(path).run().exitCode
          println()
        case _ =>
      }
    }

    return exitCode
  }

}

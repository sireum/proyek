// #Sireum
/*
 Copyright (c) 2017-2021, Robby, Kansas State University
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

  def run(path: Os.Path,
          outDirName: String,
          project: Project,
          projectName: String,
          dm: DependencyManager,
          javaOptions: ISZ[String],
          classNames: ISZ[String],
          suffixes: ISZ[String],
          packageNames: ISZ[String],
          names: ISZ[String]): Z = {

    val proyekDir = getProyekDir(path, outDirName, projectName, F)
    val projectOutDir = proyekDir / "modules"

    var testClasspath = ISZ[String]()

    for (m <- project.modules.values) {
      val mTestDir = projectOutDir / m.id / testOutDirName
      if (mTestDir.exists) {
        testClasspath = testClasspath :+ mTestDir.string
      }
    }

    for (m <- project.modules.values) {
      val mDir = projectOutDir / m.id / mainOutDirName
      if (mDir.exists) {
        testClasspath = testClasspath :+ mDir.string
      }

      testClasspath = testClasspath ++ (for (r <- ProjectUtil.moduleResources(m) ++ ProjectUtil.moduleTestResources(m)) yield r.string)
    }

    testClasspath = testClasspath :+ (dm.scalaHome / "lib" / "scala-library.jar").string

    for (lib <- dm.libMap.values) {
      testClasspath = testClasspath :+ lib.main
    }

    val classpath: ISZ[String] = for (
      cif <- dm.fetch(ISZ(s"${DependencyManager.scalaTestKey}${dm.scalaTestVersion}"))
    ) yield cif.path.string

    var args = javaOptions ++ ISZ[String](
      "-classpath", st"${(classpath, Os.pathSep)}".render,
      "org.scalatest.tools.Runner",
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
    proc"$javaExe @$argFile".at(path).console.runCheck()

    return 0
  }

}
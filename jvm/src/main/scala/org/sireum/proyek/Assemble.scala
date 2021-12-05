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

object Assemble {

  val uberHeader: ISZ[String] = ISZ(
    """@ 2>/dev/null # 2>nul & echo off & goto BOF   #""",
    """exec java -jar "$0" "$@"                      #""",
    """:BOF""",
    """java -jar "%0" %*""",
    """exit /B %errorlevel%""",
    ""
  )

  def run(path: Os.Path,
          outDirName: String,
          project: Project,
          projectName: String,
          jarName: String,
          dm: DependencyManager,
          mainClassNameOpt: Option[String],
          isNative: B,
          isUber: B): Z = {

    val trueF = (_: Os.Path) => T

    val proyekDir = getProyekDir(path, outDirName, projectName, F)
    val projectOutDir = proyekDir / "modules"

    val assembleDir = proyekDir / "assemble"
    val contentDir = assembleDir / "content"
    val jar: Os.Path = if (isUber) assembleDir / s"$jarName.bat" else assembleDir / s"$jarName.jar"

    jar.removeAll()

    println(s"Assembling ...")

    contentDir.removeAll()
    contentDir.mkdirAll()

    val metaDir = contentDir / metaInf
    metaDir.mkdirAll()

    @pure def hasScalaSource: B = {
      for (m <- project.modules.values;
           src <- ProjectUtil.moduleSources(m) ++ ProjectUtil.moduleTestSources(m);
           _ <- Os.Path.walk(src, F, F, (p: Os.Path) => p.ext === "scala")) {
        return T
      }
      return F
    }

    if (hasScalaSource) {
      (dm.scalaHome / "lib" / "scala-library.jar").unzipTo(contentDir)
      Asm.rewriteReleaseFence(contentDir)
    }

    for (lib <- dm.libMap.values) {
      Os.path(lib.main).unzipTo(contentDir)
    }

    for (m <- project.modules.values) {
      val mDir = projectOutDir / m.id / mainOutDirName
      mDir.overlayCopy(contentDir, F, F, trueF, F)
      for (r <- ProjectUtil.moduleResources(m)) {
        r.overlayCopy(contentDir, F, F, trueF, F)
      }
    }

    val manifest = metaDir / manifestMf
    val mainOpt: Option[ST] = mainClassNameOpt.map((mainClassName: String) => st"Main-Class: $mainClassName")
    manifest.writeOver(
      st"""Manifest-Version: 1.0
          |Created-By: Sireum Proyek
          |$mainOpt
          |""".render
    )

    if (isUber) {
      jar.writeOver(st"${(uberHeader, "\r\n")}".render)
      jar.chmod("+x")
    }
    contentDir.zipTo(jar)

    println(s"Wrote $jar")

    if (isNative) {
      val (platformKind, flags): (String, ISZ[String]) = Os.kind match {
        case Os.Kind.Mac => ("mac", ISZ())
        case Os.Kind.Linux => ("linux", ISZ("--static"))
        case Os.Kind.LinuxArm => ("linux/arm", ISZ("--static"))
        case Os.Kind.Win => ("win", ISZ("--static", "-H:NativeLinkerOption=Winhttp.lib"))
        case _ => halt("Unsupported operating system")
      }
      val homeBin = dm.sireumHome / "bin"
      (homeBin / "install" / "graal.cmd").call(ISZ()).console.runCheck()

      println()
      println("Building native ...")
      val tempJar = Os.temp()
      jar.copyOverTo(tempJar)
      tempJar.removeOnExit()
      Asm.eraseNonNative(jar)
      val platDir = homeBin / platformKind
      val dir = jar.up.canon
      val nativeImage: Os.Path = platDir / "graal" / "bin" / (if (Os.isWin) "native-image.cmd" else "native-image")
      val r = Os.proc((nativeImage.string +: flags) ++ ISZ[String]("--initialize-at-build-time", "--no-fallback",
        "--report-unsupported-elements-at-runtime", "-H:+ReportExceptionStackTraces", "-H:-DeadlockWatchdogExitOnTimeout",
        "-H:DeadlockWatchdogInterval=0", "--enable-url-protocols=https", "--allow-incomplete-classpath",
        "-jar", jar.string, (dir / jarName).string)).redirectErr.run()
      tempJar.copyOverTo(jar)
      if (r.exitCode =!= 0) {
        eprintln(s"Failed to generate native executable, exit code: ${r.exitCode}")
        eprintln(r.out)
        eprintln(r.err)
      }
      return r.exitCode
    }

    return 0
  }

}
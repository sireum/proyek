// #Sireum
/*
 Copyright (c) 2017-2022, Robby, Kansas State University
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

  val uberHeader: String = {
    val bs = "\\"
    st"""@ 2>/dev/null # 2>nul & echo off & goto BOF                               #
        |JAVA=java                                                                 #
        |if [ ! -z $${SIREUM_HOME} ]; then                                          #
        |  if [ -n "$$COMSPEC" -a -x "$$COMSPEC" ]; then                             #
        |    JAVA=$${SIREUM_HOME}/bin/win/java/bin/java.exe                         #
        |  elif [ "$$(uname)" = "Darwin" ]; then                                    #
        |    JAVA=$${SIREUM_HOME}/bin/mac/java/bin/java                             #
        |  elif [ "$$(expr substr $$(uname -s) 1 5)" = "Linux" ]; then               #
        |    if [ "$$(uname -m)" = "aarch64" ]; then                                #
        |      JAVA=$${SIREUM_HOME}/bin/linux/arm/java/bin/java                     #
        |    else                                                                  #
        |      JAVA=$${SIREUM_HOME}/bin/linux/java/bin/java                         #
        |    fi                                                                    #
        |  fi                                                                      #
        |fi                                                                        #
        |exec $${JAVA} -jar "$$0" "$$@"                                               #
        |:BOF
        |setlocal
        |set JAVA=java
        |if defined SIREUM_HOME set JAVA=%SIREUM_HOME%${bs}bin${bs}win${bs}java${bs}bin${bs}java.exe
        |%JAVA% -jar "%0" %*
        |exit /B %errorlevel%
        |""".render
  }

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
    val jar = assembleDir / s"$jarName.jar"
    val uber = assembleDir / s"${jar.name}.bat"

    jar.removeAll()
    uber.removeAll()

    println(s"Assembling ...")

    contentDir.removeAll()
    contentDir.mkdirAll()

    val metaDir = contentDir / metaInf
    metaDir.mkdirAll()

    if (Proyek.hasScalaSource(project)) {
      (dm.scalaHome / "lib" / "scala-library.jar").unzipTo(contentDir)
      Asm.rewriteReleaseFence(contentDir)
    }

    var testLibNames = HashSet.empty[String]
    for (cif <- dm.fetch(ISZ(s"${DependencyManager.scalaTestKey}${dm.scalaTestVersion}"))) {
      val name = DependencyManager.libName(cif)
      if (!DependencyManager.ignoredLibraryNames.contains(name) && !ops.StringOps(name).startsWith("org.scala-lang.modules.scala-xml_")) {
        testLibNames = testLibNames + name
      }
    }

    for (lib <- dm.libMap.values if !testLibNames.contains(lib.name)) {
      Os.path(lib.main).unzipTo(contentDir)
    }

    for (m <- project.modules.values) {
      val mDir = projectOutDir / m.id / mainOutDirName
      mDir.overlayCopy(contentDir, F, F, trueF, F)
      for (r <- ProjectUtil.moduleResources(m)) {
        r.overlayCopy(contentDir, F, F, trueF, F)
      }
    }

    @pure def filterMetaFile(p: Os.Path): B = {
      ops.StringOps(p.ext).toUpper match {
        case string"RSA" => return T
        case string"SF" => return T
        case string"DES" => return T
        case string"DSA" => return T
        case _ =>
      }
      return p.name == "native-image.properties"
    }

    for (f <- Os.Path.walk(metaDir, F, F, filterMetaFile _)) {
      f.removeAll()
    }

    val manifest = metaDir / manifestMf
    val mainOpt: Option[ST] = mainClassNameOpt.map((mainClassName: String) => st"Main-Class: $mainClassName")
    manifest.writeOver(
      st"""Manifest-Version: 1.0
          |Created-By: Sireum Proyek
          |$mainOpt
          |""".render
    )

    contentDir.zipTo(jar)

    if (isUber) {
      val temp = Os.temp()
      temp.removeOnExit()
      temp.writeOver(ops.StringOps(uberHeader).replaceAllLiterally("\n", "\r\n"))
      uber.combineFrom(ISZ(temp, jar))
      uber.chmod("+x")
      println(s"Wrote $jar[.bat]")
    } else {
      println(s"Wrote $jar")
    }

    if (isNative) {
      val (platformKind, flags): (String, ISZ[String]) = Os.kind match {
        case Os.Kind.Mac => ("mac", ISZ())
        case Os.Kind.Linux => ("linux", ISZ())
        case Os.Kind.LinuxArm => ("linux/arm", ISZ())
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
      if (r.exitCode != 0) {
        eprintln(s"Failed to generate native executable, exit code: ${r.exitCode}")
        eprintln(r.out)
        eprintln(r.err)
      }
      return r.exitCode
    }

    return 0
  }

}
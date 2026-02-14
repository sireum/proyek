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

  val graalRtPackagesOrClasses: ISZ[String] = ISZ[String](
    "com.zaxxer.nuprocess.internal",
    "com.zaxxer.nuprocess.osx",
    "com.zaxxer.nuprocess.linux",
    "com.zaxxer.nuprocess.windows",
    "com.zaxxer.nuprocess",
    "com.sun.jna.internal",
    "com.sun.jna",
    "org.jline.nativ",
    "org.jline.nativ.Kernel32$MENU_EVENT_RECORD",
    "org.jline.terminal.impl.jna.win",
    "coursierapi.internal.jniutils",
    "org.sireum.parser.SireumAntlr3ParserUtil$"
  )

  val graalOpts: ISZ[String] = ISZ[String](
    "--initialize-at-build-time",
    "--enable-url-protocols=https",
    "--no-fallback",
    "-H:+ReportExceptionStackTraces",
    st"--initialize-at-run-time=${(graalRtPackagesOrClasses, ",")}".render
  )

  def installMusl(sireumHome: Os.Path): Option[Os.Path] = {
    val homeBin = sireumHome / "bin"
    val muslBinOpt: Option[Os.Path] = Os.kind match {
      case Os.Kind.Linux => Some(homeBin / "linux" / "musl" / "bin")
      case Os.Kind.LinuxArm => Some(homeBin / "linux" / "arm" / "musl" / "bin")
      case _ => None()
    }
    if (muslBinOpt.nonEmpty && !muslBinOpt.get.exists) {
      (homeBin / "install" / "musl.cmd").call(ISZ()).console.runCheck()
    }
    return muslBinOpt
  }

  def nativ(sireumHome: Os.Path, jar: Os.Path, genScript: B): Z = {
    val (platformKind, flags): (String, ISZ[String]) = Os.kind match {
      case Os.Kind.Mac => ("mac", ISZ())
      case Os.Kind.Linux => ("linux", ISZ("--static", "--libc=musl"))
      case Os.Kind.LinuxArm => ("linux/arm", ISZ("--static", "--libc=musl"))
      case Os.Kind.Win => ("win", ISZ("-H:NativeLinkerOption=Winhttp.lib"))
      case _ => halt("Unsupported operating system")
    }
    val homeBin = sireumHome / "bin"

    println()
    if (!genScript) {
      println("Building native ...")
    }
    val tempJar = Os.tempFix("", ".jar")
    jar.copyOverTo(tempJar)
    Asm.eraseNonNative(tempJar)
    val platDir = homeBin / platformKind
    val dir = jar.up.canon
    var nativeImage: Os.Path = platDir / "graal" / "bin" / (if (Os.isWin) "native-image.cmd" else "native-image")
    if (!nativeImage.exists) {
      val javaHome = Os.javaHomeOpt(Os.kind, Some(sireumHome)).get
      val nik = javaHome / "bin" / nativeImage.name
      if (nik.exists) {
        nativeImage = nik
      } else {
        (homeBin / "install" / "graal.cmd").call(ISZ()).console.runCheck()
      }
    }
    val jarName = ops.StringOps(jar.name).substring(0, jar.name.size - 4)
    val out = (dir / jarName).string
    var p = Os.proc((nativeImage.string +: flags) ++ graalOpts ++
      ISZ[String]("-jar", tempJar.string, out)).redirectErr
    installMusl(sireumHome) match {
      case Some(muslBin) => p = p.env(ISZ("PATH" ~> s"${Os.env("PATH").get}:$muslBin"))
      case _ =>
    }
    if (genScript) {
      if (Os.isWin) {
        val script = jar.up / "build-native.bat"
        script.writeOver(
          st"""${(p.cmds, " ")}
              |echo Wrote $out.exe""".render
        )
        println(s"Wrote $script")
      } else {
        val script = jar.up / "build-native"
        script.writeOver(
          st"""${(p.cmds, " ")}
              |echo "Wrote $out"""".render
        )
        script.chmod("+x")
        println(s"Wrote $script")
      }
      return 0
    } else {
      val r = p.run()
      tempJar.removeAll()
      if (r.exitCode != 0) {
        eprintln(s"Failed to generate native executable, exit code: ${r.exitCode}")
        eprintln(r.out)
        eprintln(r.err)
      }
      return r.exitCode
    }
  }

  def run(path: Os.Path,
          outDirName: String,
          project: Project,
          projectName: String,
          jarName: String,
          noDeps: B,
          dm: DependencyManager,
          mainClassNameOpt: Option[String],
          isNative: B,
          isNativeScript: B,
          isUber: B,
          includeSources: B,
          includeTests: B,
          excludedDeps: ISZ[(String, String)]): Z = {

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

    @pure def isExcluded(org: String, module: String): B = {
      for (p <- excludedDeps) {
        if (ops.StringOps(org).startsWith(p._1) && ops.StringOps(module).startsWith(p._2)) {
          return T
        }
      }
      return F
    }

    if (Proyek.hasScalaSource(project)) {
      if (!noDeps && !isExcluded("org.scala-lang", "scala-library")) {
        (dm.scalaHome / "lib" / "scala-library.jar").unzipTo(contentDir)
      }
      if (Proyek.hasSlangSource(project)) {
        Asm.rewriteReleaseFence(contentDir)
      }
    }

    var testLibNames = HashSet.empty[String]
    if (!includeTests) {
      for (cif <- dm.fetch(ISZ(s"${DependencyManager.scalaTestKey}${dm.scalaTestVersion}"))) {
        val name = DependencyManager.libName(cif)
        if (!DependencyManager.ignoredLibraryNames.contains(name) && !ops.StringOps(name).startsWith("org.scala-lang.modules.scala-xml_")) {
          testLibNames = testLibNames + name
        }
      }
    }

    if (!noDeps) {
      for (lib <- dm.libMap.values if !testLibNames.contains(lib.name) &&
        (lib.name != "org.scala-lang.scalap" || Proyek.hasScalaSource(project))) {
        if (!isExcluded(lib.org, lib.module)) {
          Os.path(lib.main).unzipTo(contentDir)
          if (includeSources) {
            lib.sourcesOpt match {
              case Some(src) => Os.path(src).unzipTo(contentDir)
              case _ =>
            }
          }
        }
      }
    }

    for (m <- project.modules.values) {
      val mDir = projectOutDir / m.id / mainOutDirName
      mDir.overlayCopy(contentDir, F, F, trueF, F)
      if (includeSources) {
        val srcs = ProjectUtil.moduleSources(m)
        for (src <- srcs) {
          src.overlayCopy(contentDir, F, F, trueF, T)
        }
      }
      if (includeTests) {
        val tDir = projectOutDir / m.id / testOutDirName
        tDir.overlayCopy(contentDir, F, F, trueF, F)

        if (includeSources) {
          for (src <- ProjectUtil.moduleTestSources(m)) {
            src.overlayCopy(contentDir, F, F, trueF, F)
          }
        }
      }
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

    if (isNative || isNativeScript) {
      nativ(dm.sireumHome, jar, isNativeScript)
    }

    return 0
  }

}
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

object Compile {

  @enum object CompileStatus {
    "Compiled"
    "Skipped"
    "Error"
  }

  @datatype class CompileModuleProcessor(val root: Os.Path,
                                         val module: Module,
                                         val force: B,
                                         val par: B,
                                         val sha3: B,
                                         val followSymLink: B,
                                         val outDir: Os.Path,
                                         val javaHome: Os.Path,
                                         val scalaHome: Os.Path,
                                         val javacOptions: ISZ[String],
                                         val scalacOptions: ISZ[String],
                                         val scalacPlugin: Os.Path,
                                         val isJs: B) extends ModuleProcessor[(CompileStatus.Type, String)] {

    @pure override def fileFilter(file: Os.Path): B = {
      var r: B = file.ext == "scala"
      if (!isJs) {
        r = r || file.ext == "java"
      }
      return r
    }

    override def process(o: (CompileStatus.Type, String),
                         shouldProcess: B,
                         dm: DependencyManager,
                         sourceFiles: ISZ[Os.Path],
                         testSourceFiles: ISZ[Os.Path]): ((CompileStatus.Type, String), B) = {

      if (!shouldProcess) {
        return ((CompileStatus.Skipped, ""), F)
      }

      var classpath: ISZ[Os.Path] = for (lib <- dm.fetchTransitiveLibs(module)) yield Os.path(lib.main)
      if (isJs) {
        classpath = dm.fetch(ISZ(s"org.scala-js::scalajs-library:${dm.scalaJsVersion}"))(0).path +: classpath
      }
      classpath = classpath ++ (
        for (mDep <- dm.computeTransitiveDeps(module) if (outDir / mDep / mainOutDirName).exists) yield
          outDir / mDep / mainOutDirName
        )

      var plugins = ISZ(scalacPlugin)
      if (isJs) {
        plugins = plugins :+
          dm.fetch(ISZ(s"${DependencyManager.scalaJsKey}${dm.scalaJsVersion}"))(0).path
      }
      val scOptions = scalacOptions :+ st"-Xplugin:${(plugins, ",")}".render

      val mainOutDir = outDir / module.id / mainOutDirName
      classpath = mainOutDir +: classpath
      mainOutDir.removeAll()
      mainOutDir.mkdirAll()
      val (mainOk, mainOut) = runCompilers(
        mid = module.id,
        category = "main",
        javaHome = javaHome,
        scalaHome = scalaHome,
        scalacOptions = scOptions,
        javacOptions = javacOptions,
        classpath = classpath,
        sourceFiles = sourceFiles,
        outDir = mainOutDir
      )

      if (mainOk) {
        if (testSourceFiles.nonEmpty) {
          val testOutDir = outDir / module.id / testOutDirName

          classpath = classpath ++ (
            for (mDep <- dm.computeTransitiveDeps(module) if (outDir / mDep / testOutDirName).exists) yield
              outDir / mDep / testOutDirName
            )
          classpath = testOutDir +: classpath
          testOutDir.removeAll()
          testOutDir.mkdirAll()

          val (testOk, testOut) = runCompilers(
            mid = module.id,
            category = "test",
            javaHome = javaHome,
            scalaHome = scalaHome,
            scalacOptions = scOptions,
            javacOptions = javacOptions,
            classpath = classpath,
            sourceFiles = testSourceFiles,
            outDir = testOutDir
          )
          if (testOk) {
            return ((CompileStatus.Compiled, s"$mainOut$testOut"), T)
          } else {
            return ((CompileStatus.Error, s"$mainOut$testOut"), F)
          }
        } else {
          return ((CompileStatus.Compiled, mainOut), T)
        }
      } else {
        return ((CompileStatus.Error, mainOut), F)
      }
    }

  }

  def run(path: Os.Path,
          outDirName: String,
          project: Project,
          projectName: String,
          dm: DependencyManager,
          javacOptions: ISZ[String],
          scalacOptions: ISZ[String],
          isJs: B,
          followSymLink: B,
          fresh: B,
          par: B,
          sha3: B,
          ignoreRuntime: B,
          recompileModuleIds: ISZ[String]): Z = {

    val proyekDir = getProyekDir(path, outDirName, projectName, isJs)

    val projectOutDir = proyekDir / "modules"

    val versionsCache = proyekDir / "versions.json"
    val projectCache = proyekDir / "proyek.json"

    var versions = dm.versions
    if (ignoreRuntime) {
      versions = versions -- ISZ(DependencyManager.libraryKey)
    }

    val versionsChanged: B = if (fresh) {
      T
    } else {
      loadVersions(versionsCache) match {
        case Some(m) =>
          val r = m != versions
          if (r) {
            println("Dependency version changes detected ...")
            println()
          }
          r
        case _ => T
      }
    }

    val projectNoPub = project.stripPubInfo

    val projectChanged: B = if (fresh) {
      T
    } else {
      ProjectUtil.load(projectCache) match {
        case Some(pc) =>
          val r = !(projectNoPub <= pc)
          if (r) {
            println("Project changes detected ...")
            println()
          }
          r
        case _ => T
      }
    }

    val compileAll = fresh || versionsChanged || projectChanged

    if (compileAll) {
      storeVersions(versionsCache, versions)
      ProjectUtil.store(projectCache, projectNoPub)
    }

    if (compileAll) {
      println("Fresh compilation ...")
      println()
      projectOutDir.removeAll()
    }
    projectOutDir.mkdirAll()

    val target: Target.Type = if (isJs) Target.Js else Target.Jvm
    var modules: ISZ[(String, B)] = for (n <- project.poset.rootNodes) yield (n, compileAll)
    var compiledModuleIds = HashSet.empty[String]
    val recompileIds = HashSet ++ recompileModuleIds
    while (modules.nonEmpty) {
      var nexts = ISZ[(Module, B)]()
      var newModules = HashSMap.empty[String, B]
      for (p <- modules) {
        val m = dm.getModule(p._1)
        if (ops.ISZOps(m.deps).forall((mDep: String) => compiledModuleIds.contains(mDep))) {
          if (m.hasTarget(target)) {
            nexts = nexts :+ ((m, p._2))
          }
        } else {
          if (m.hasTarget(target)) {
            newModules = newModules + p
          }
        }
      }
      if (nexts.nonEmpty) {
        val nextIds: ISZ[String] = for (next <- nexts) yield
          if (!compileAll && !next._2 && recompileIds.contains(next._1.id)) s"${next._1.id}*"
          else next._1.id
        println(st"Compiling module${if (nextIds.size > 1) "s" else ""}: ${(nextIds, ", ")} ...".render)
        val compileModule = (pair: (Module, B)) => CompileModuleProcessor(
          root = path,
          module = pair._1,
          force = pair._2 || recompileIds.contains(pair._1.id),
          par = par,
          sha3 = sha3,
          followSymLink = followSymLink,
          outDir = projectOutDir,
          javaHome = dm.javaHome,
          scalaHome = dm.scalaHome,
          javacOptions = javacOptions,
          scalacOptions = scalacOptions,
          scalacPlugin = dm.scalacPlugin,
          isJs = isJs
        ).run((CompileStatus.Compiled, ""), dm)
        val r: ISZ[(CompileStatus.Type, String)] =
          if (par) ops.ISZOps(nexts).mParMap(compileModule)
          else for (next <- nexts) yield compileModule(next)
        var ok = T
        for (p <- r) {
          if (p._1 == CompileStatus.Error) {
            ok = F
          }
          print(p._2)
        }
        if (!ok) {
          return -1
        }
        for (p <- ops.ISZOps(nextIds).zip(r)) {
          val (mid, (status, _)) = p
          for (mDep <- project.poset.childrenOf(mid).elements) {
            val compile: B = status == CompileStatus.Compiled
            newModules.get(mDep) match {
              case Some(b) => newModules = newModules + mDep ~> (b | compile)
              case _ => newModules = newModules + mDep ~> compile
            }
          }
        }

        println()
        compiledModuleIds = compiledModuleIds ++ nextIds
      }
      modules = newModules.entries
    }
    return 0
  }

  def runCompilers(mid: String,
                   category: String,
                   javaHome: Os.Path,
                   scalaHome: Os.Path,
                   scalacOptions: ISZ[String],
                   javacOptions: ISZ[String],
                   classpath: ISZ[Os.Path],
                   sourceFiles: ISZ[Os.Path],
                   outDir: Os.Path): (B, String) = {

    if (sourceFiles.isEmpty) {
      return (T, "")
    }

    var scalaArgs = ISZ[String]("-classpath", st"${(classpath, Os.pathSep)}".render)
    scalaArgs = scalaArgs :+ "-d" :+ outDir.string
    var javaArgs = scalaArgs

    var ok = T
    var sb = ISZ[ST]()

    var javaSources = ISZ[String]()
    var numOfSlangFiles: Z = 0
    var numOfScalaFiles: Z = 0
    for (f <- sourceFiles) {
      f.ext match {
        case string"scala" =>
          var cs = ISZ[C]()
          for (c <- f.readCStream.takeWhile((c: C) => c =!= '\n')) {
            if (!c.isWhitespace) {
              cs = cs :+ c
            }
          }
          if (ops.StringOps(conversions.String.fromCis(cs)).contains("#Sireum")) {
            numOfSlangFiles = numOfSlangFiles + 1
          } else {
            numOfScalaFiles = numOfScalaFiles + 1
          }
        case _ => javaSources = javaSources :+ f.string
      }
    }
    val numOfJavaFiles = javaSources.size

    (numOfSlangFiles, numOfScalaFiles, numOfJavaFiles) match {
      case (_, z"0", z"0") => sb = sb :+ st"* Compiled $numOfSlangFiles Slang $mid $category source file${if (numOfSlangFiles > 1) "s" else ""}\n"
      case (z"0", _, z"0") => sb = sb :+ st"* Compiled $numOfScalaFiles Scala $mid $category source file${if (numOfScalaFiles > 1) "s" else ""}\n"
      case (z"0", z"0", _) => sb = sb :+ st"* Compiled $numOfJavaFiles Java $mid $category source file${if (numOfJavaFiles > 1) "s" else ""}\n"
      case (_, _, z"0") => sb = sb :+ st"* Compiled $numOfSlangFiles Slang and $numOfScalaFiles Scala $mid $category source files\n"
      case (_, z"0", _) => sb = sb :+ st"* Compiled $numOfSlangFiles Slang and $numOfJavaFiles Java $mid $category source files\n"
      case (z"0", _, _) => sb = sb :+ st"* Compiled $numOfScalaFiles Scala and $numOfJavaFiles Java $mid $category source files\n"
      case (_, _, _) => sb = sb :+ st"* Compiled $numOfSlangFiles Slang, $numOfScalaFiles Scala, and $numOfJavaFiles Java $mid $category source files\n"
    }

    if (numOfSlangFiles > 0 || numOfScalaFiles > 0) {
      val scalac: Os.Path = scalaHome / "bin" / (if (Os.isWin) "scalac.bat" else "scalac")
      scalaArgs = scalaArgs ++ scalacOptions
      scalaArgs = scalaArgs ++ (for (f <- sourceFiles) yield f.string)

      val argFile = outDir.up / s"scalac-args-$category"
      argFile.writeOver(st"${(scalaArgs, "\n")}".render)
      val r = proc"$scalac @${argFile.name}".at(argFile.up.canon).run()
      ok = r.ok
      sb = sb :+ st"${r.out}"
      sb = sb :+ st"${r.err}"
    }

    if (ok) {
      if (javaSources.nonEmpty) {
        javaArgs = javaArgs ++ javacOptions
        javaArgs = javaArgs ++ javaSources
        val argFile = outDir.up / s"javac-args-$category"
        argFile.writeOver(st"${(javaArgs, "\n")}".render)
        val javac: Os.Path = javaHome / "bin" / (if (Os.isWin) "javac.exe" else "javac")
        val r = proc"$javac @${argFile.name}".at(argFile.up.canon).run()
        sb = sb :+ st"${r.out}"
        sb = sb :+ st"${r.err}"
        return (r.ok, st"${(sb, "")}".render)
      } else {
        return (T, st"${(sb, "")}".render)
      }
    } else {
      return (F, st"${(sb, "")}".render)
    }
  }

}
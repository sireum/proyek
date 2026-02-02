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
import org.sireum.project.{DependencyManager, ProjectUtil}

object Proyek {

  val metaInf: String = "META-INF"
  val manifestMf: String = "MANIFEST.MF"

  val mainOutDirName: String = "classes"
  val testOutDirName: String = "test-classes"
  val sourcesOutDirName: String = "sources"
  val javadocOutDirName: String = "javadoc"

  @pure def getProyekDir(path: Os.Path, outDirName: String, projectName: String, isJs: B): Os.Path = {
    return path / outDirName / s"$projectName${if (isJs) "-js" else ""}"
  }

  @pure def normalizePath(path: String): String = {
    if (Os.isWin) {
      return ops.StringOps(path).replaceAllChars('\\', '/')
    } else {
      return path
    }
  }

  @pure def relUri(from: Os.Path, to: Os.Path): String = {
    return normalizePath(from.relativize(to).string)
  }

  def loadVersions(path: Os.Path): Option[HashSMap[String, String]] = {
    if (!path.isFile) {
      return None()
    }
    val jsonParser = Json.Parser.create(path.read)
    val m = jsonParser.parseHashSMap(jsonParser.parseString _, jsonParser.parseString _)
    return if (jsonParser.errorOpt.isEmpty) Some(m) else None()
  }

  def storeVersions(path: Os.Path, versions: HashSMap[String, String]): Unit = {
    path.up.mkdirAll()
    path.writeOver(
      Json.Printer.printHashSMap(F, versions, Json.Printer.printString _, Json.Printer.printString _).render
    )
  }

  def getProject(sireumHome: Os.Path, path: Os.Path, jsonOpt: Option[String], projectOpt: Option[String]): Option[project.Project] = {
    var prj = project.Project.empty
    var loaded = F

    {
      jsonOpt match {
        case Some(p) =>
          val f = Os.path(p)
          if (!f.isFile) {
            eprintln(s"$p is not a file")
            return None()
          }
          project.JSON.toProject(f.read) match {
            case Either.Left(pr) =>
              prj = pr
              loaded = T
            case _ =>
              eprintln(s"Ill-formed JSON project file $p")
              return None()
          }
          println()
        case _ =>
      }
    }

    if (!loaded) {
      val f: Os.Path = projectOpt match {
        case Some(p) => Os.path(p)
        case _ =>
          val projectStandalone = path / "bin" / "project-standalone.cmd"
          if (projectStandalone.exists) {
            projectStandalone
          } else {
            path / "bin" / "project.cmd"
          }
      }
      if (!f.isFile) {
        eprintln(s"$f is not a file")
        return None()
      }
      if (f.ext != "cmd") {
        eprintln(s"$f is not a .cmd Slash script file")
        return None()
      }
      val cmds: ISZ[String] =
        if (Os.isWin) ISZ[String]("cmd", "/C", f.name, "json")
        else ISZ[String]("bash", s"./${f.name}", "json")
      val r = Os.proc(cmds).at(f.up.canon).env(ISZ("SIREUM_HOME" ~> sireumHome.string)).
        console.outLineAction((s: String) => project.ProjectUtil.projectJsonLine(s).isEmpty).run()
      if (r.ok) {
        project.ProjectUtil.projectJsonLine(r.out) match {
          case Some(line) =>
            project.JSON.toProject(line) match {
              case Either.Left(pr) =>
                prj = pr
              case _ =>
                eprintln(s"Ill-defined project file $f producing:")
                eprintln(line)
                return None()
            }
          case _ =>
            eprintln(s"Failed to load project from $f")
            println(r.out)
            eprintln(r.err)
            return None()
        }
      } else {
        eprintln(s"Failed to load project from $f")
        println(r.out)
        eprintln(r.err)
        return None()
      }
    }

    val openDeps = prj.openDeps
    if (openDeps.nonEmpty) {
      for (openDep <- openDeps.entries) {
        val (mid, deps) = openDep
        eprintln(st"Module $mid depends on undefined modules: ${(deps, ", ")}".render)
      }
      return None()
    }

    val illTargets = prj.illTargets
    if (illTargets.nonEmpty) {
      for (illTarget <- illTargets.entries) {
        val mid = illTarget._1
        for (illTargetDep <- illTarget._2.entries) {
          val (mDep, targets) = illTargetDep
          eprintln(st"Module $mid depends on undefined target(s) of module $mDep: ${(targets, ", ")}".render)
        }
      }
      return None()
    }

    ;{
      var ok = T
      for (m <- prj.modules.values) {
        if (ProjectUtil.moduleSources(m).isEmpty && ProjectUtil.moduleTestSources(m).isEmpty) {
          eprintln()
          eprintln(s"Module ${m.id} does not have any source paths that exist among:")
          for (p <- for (source <- m.sources ++ m.testSources) yield ProjectUtil.pathSep(ProjectUtil.moduleBasePath(m), source)) {
            eprintln(s"* $p")
          }
          ok = F
        }
      }
      if (!ok) {
        return None()
      }
    }

    return Some(prj)
  }

  def getVersions(prj: project.Project, path: Os.Path, versions: ISZ[String], default: ISZ[(String, String)]): Option[HashSMap[String, String]] = {
    val files: ISZ[Os.Path] = if (versions.nonEmpty) {
      for (v <- versions) yield Os.path(v)
    } else {
      val f = path / "versions.properties"
      if (f.exists) ISZ(f) else ISZ()
    }

    var props = HashSMap.empty[String, String]
    props = props ++ default
    for (f <- files) {
      if (!f.isFile) {
        eprintln(s"$f is not a file")
        return None()
      } else {
        props = props ++ (for (p <- f.properties.entries) yield (ops.StringOps(p._1).replaceAllChars('%', ':'), p._2))
      }
    }

    var delProps = ISZ(DependencyManager.macrosKey, DependencyManager.testKey, DependencyManager.librarySharedKey, DependencyManager.libraryKey)
    for (m <- prj.modules.values; ivyDep <- m.ivyDeps) {
      ivyDep match {
        case DependencyManager.macrosKey => delProps = delProps - DependencyManager.macrosKey
        case DependencyManager.testKey => delProps = delProps - DependencyManager.testKey
        case DependencyManager.librarySharedKey => delProps = delProps - DependencyManager.librarySharedKey
        case DependencyManager.libraryKey => delProps = delProps - DependencyManager.libraryKey
        case _ =>
      }
    }
    props = props -- delProps
    return Some(props)
  }

  @pure def firstCompactLineOps(cs: Jen[C]): ops.StringOps = {
    var cis = ISZ[C]()
    for (c <- cs.takeWhile((c: C) => c != '\n') if !c.isWhitespace) {
      cis = cis :+ c
    }
    return ops.StringOps(conversions.String.fromCis(cis))
  }

  @pure def hasScalaSource(project: org.sireum.project.Project): B = {
    for (m <- project.modules.values;
         src <- ProjectUtil.moduleSources(m) ++ ProjectUtil.moduleTestSources(m);
         _ <- Os.Path.walk(src, F, F, (p: Os.Path) => p.ext == "scala")) {
      return T
    }
    return F
  }

  @pure def hasSlangSource(project: org.sireum.project.Project): B = {
    for (m <- project.modules.values;
         src <- ProjectUtil.moduleSources(m) ++ ProjectUtil.moduleTestSources(m);
         f <- Os.Path.walk(src, F, F, (p: Os.Path) => p.ext == "scala") if
           lang.parser.Parser.detectSlang(Some(f.toUri), f.readLineStream.take(1).mkString("\n"))._1) {
      return T
    }
    return F
  }
}

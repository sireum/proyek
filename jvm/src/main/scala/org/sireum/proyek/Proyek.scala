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
import org.sireum.project.{Project, Module}


object Proyek {

  @enum object CompileStatus {
    'Compiled
    'Skipped
    'Error
  }

  @datatype class Lib(val name: String,
                      val org: String,
                      val module: String,
                      val main: String,
                      val sourcesOpt: Option[String],
                      val javadocOpt: Option[String])

  @record class DependencyManager(project: Project, versions: HashSMap[String, String], withSource: B, withDoc: B) {

    val ivyDeps: HashSMap[String, String] = {
      var r = HashSMap.empty[String, String]
      for (m <- project.modules.values) {
        for (ivyDep <- m.ivyDeps) {
          val v = getVersion(ivyDep)
          r = r + ivyDep ~> s"$ivyDep$v"
          //val ivyDepOps = ops.StringOps(ivyDep)
          //if (ivyDepOps.endsWith("::")) {
          //  val dep = s"${ivyDepOps.substring(0, ivyDep.size - 2)}$sjsSuffix:"
          //  r = r + dep ~> s"$dep$v"
          //}
        }
      }
      r
    }

    val libMap: HashSMap[String, Lib] = {
      var r = HashSMap.empty[String, Lib]
      for (cif <- Coursier.fetchClassifiers(ivyDeps.values, buildClassifiers(withSource, withDoc))) {
        val name = libName(cif)
        val p = cif.path
        val pNameOps = ops.StringOps(p.string)
        if (!ignoredLibraryNames.contains(name)) {
          var lib: Lib = r.get(name) match {
            case Some(l) => l
            case _ => Lib(name, cif.org, cif.module, "", None(), None())
          }
          if (pNameOps.endsWith(sourceJarSuffix)) {
            lib = lib(sourcesOpt = Some(p.string))
          } else if (pNameOps.endsWith(docJarSuffix)) {
            lib = lib(javadocOpt = Some(p.string))
          } else if (pNameOps.endsWith(jarSuffix)) {
            lib = lib(main = p.string)
          } else {
            halt(s"Expecting a file with .jar extension but found '$p'")
          }
          r = r + name ~> lib
        }
      }
      r
    }

    var tLibMap: HashMap[String, ISZ[Lib]] = HashMap.empty

    var dLibMap: HashMap[String, ISZ[Lib]] = HashMap.empty

    @pure def getVersion(ivyDep: String): String = {
      versions.get(ops.StringOps(ivyDep).replaceAllChars(':', '%')) match {
        case Some(v) => return v
        case _ => halt(s"Could not find version information for '$ivyDep' in $versions")
      }
    }

    @pure def getModule(id: String): Module = {
      project.modules.get(id) match {
        case Some(m) => return m
        case _ => halt(s"Could not find module with ID '$id'")
      }
    }

    @memoize def computeTransitiveDeps(m: Module): ISZ[String] = {
      var r = HashSSet.empty[String]
      for (mDep <- m.deps) {
        r = r + mDep ++ computeTransitiveDeps(getModule(mDep))
      }
      return r.elements
    }

    @memoize def computeTransitiveIvyDeps(m: Module): ISZ[String] = {
      var r = HashSSet.empty[String]
      for (mid <- m.deps) {
        r = r ++ computeTransitiveIvyDeps(project.modules.get(mid).get)
      }
      for (id <- m.ivyDeps) {
        r = r + ivyDeps.get(id).get
        //val idOps = ops.StringOps(id)
        //if (idOps.endsWith("::")) {
        //  val dep = s"${idOps.substring(0, id.size - 2)}$sjsSuffix:"
        //  r = r + ivyDeps.get(dep).get
        //}
      }
      return r.elements
    }

    def fetchTransitiveLibs(m: Module): ISZ[Lib] = {
      tLibMap.get(m.id) match {
        case Some(libs) => return libs
        case _ =>
      }
      val r: ISZ[Lib] =
        for (cif <- Coursier.fetch(computeTransitiveIvyDeps(m)) if !ignoredLibraryNames.contains(libName(cif))) yield libMap.get(libName(cif)).get
      tLibMap = tLibMap + m.id ~> r
      return r
    }

    def fetchDiffLibs(m: Module): ISZ[Lib] = {
      dLibMap.get(m.id) match {
        case Some(libs) => return libs
        case _ =>
      }
      var s = HashSSet ++ fetchTransitiveLibs(m)
      for (mDep <- m.deps) {
        s = s -- fetchTransitiveLibs(getModule(mDep))
      }
      val r = s.elements
      dLibMap = dLibMap + m.id ~> r
      return r
    }
  }

  val jarSuffix: String = ".jar"
  val sourceJarSuffix: String = "-sources.jar"
  val docJarSuffix: String = "-javadoc.jar"
  val sjsSuffix: String = "_sjs1"
  val ignoredLibraryNames: HashSet[String] = HashSet ++ ISZ[String](
    "org.scala-lang.scala-library", "org.scala-lang.scala-reflect", "org.scala-lang.scala-compiler"
  )
  val ignoredPathNames: HashSet[String] = HashSet ++ ISZ[String](
    ".git", ".DS_Store"
  )
  val cacheSuffix: String = "-inc-cache.zip"
  val mainOutDirName: String = "classes"
  val mainCacheName: String = s"$mainOutDirName$cacheSuffix"
  val testOutDirName: String = "test-classes"
  val testCacheName: String = s"$testOutDirName$cacheSuffix"
  val sourcesOutDirName: String = "sources"
  val metaInf: String = "META-INF"
  val manifestMf: String = "MANIFEST.MF"


  def assemble(path: Os.Path,
               outDirName: String,
               project: Project,
               projectName: String,
               jarName: String,
               dm: DependencyManager,
               scalaHome: Os.Path,
               mainClassNameOpt: Option[String]): Z = {

    val trueF = (_: Os.Path) => T

    val proyekDir = getProyekDir(path, outDirName, projectName)
    val projectOutDir = proyekDir / "modules"

    val assembleDir = proyekDir / "assemble"
    val contentDir = assembleDir / "content"
    val jar = assembleDir / s"$jarName.jar"
    jar.removeAll()

    println(s"Assembling ...")

    contentDir.removeAll()
    contentDir.mkdirAll()

    val metaDir = contentDir / metaInf
    metaDir.mkdirAll()

    (scalaHome / "lib" / "scala-library.jar").unzipTo(contentDir)

    for (lib <- dm.libMap.values) {
      Os.path(lib.main).unzipTo(contentDir)
    }

    for (m <- project.modules.values) {
      val mDir = projectOutDir / m.id / mainOutDirName
      mDir.overlayCopy(contentDir, F, F, trueF, F)
      for (r <- m.resources) {
        Os.path(r).overlayCopy(contentDir, F, F, trueF, F)
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

    contentDir.zipTo(jar)

    Asm.rewriteReleaseFence(jar)

    println(s"Wrote $jar")

    return 0
  }

  def compile(path: Os.Path,
              outDirName: String,
              project: Project,
              projectName: String,
              dm: DependencyManager,
              javaHome: Os.Path,
              scalaHome: Os.Path,
              scalacPlugin: Os.Path,
              followSymLink: B,
              fresh: B,
              par: B,
              sha3: B): Z = {

    val proyekDir = getProyekDir(path, outDirName, projectName)

    val projectOutDir = proyekDir / "modules"

    val versionsCache = proyekDir / "versions.json"
    val projectCache = proyekDir / "proyek.json"

    var compileAll: B = if (versionsCache.exists) {
      val jsonParser = Json.Parser.create(versionsCache.read)
      val m = jsonParser.parseHashSMap(jsonParser.parseString _, jsonParser.parseString _)
      jsonParser.errorOpt.nonEmpty || m != dm.versions
    } else {
      T
    }

    val projectNoPub: Project = project(
      modules = HashSMap ++ (for (p <- project.modules.entries) yield (p._1, p._2(publishInfoOpt = None())))
    )

    if (!compileAll) {
      if (projectCache.exists) {
        val parser = org.sireum.project.JSON.Parser(projectCache.read)
        val m = parser.parser.parseHashSMap(parser.parser.parseString _, parser.parseModule _)
        compileAll = parser.errorOpt.nonEmpty || m != projectNoPub.modules
      } else {
        compileAll = T
      }
    }

    if (compileAll) {
      versionsCache.writeOver(
        Json.Printer.printHashSMap(F, dm.versions, Json.Printer.printString _, Json.Printer.printString _).render
      )
      projectCache.writeOver(Json.Printer.printHashSMap(F, projectNoPub.modules, Json.Printer.printString _,
        org.sireum.project.JSON.Printer.printModule _).render)
    }

    val scalacOptions = ISZ[String](
      "-target:jvm-1.8",
      "-deprecation",
      "-Yrangepos",
      "-Ydelambdafy:method",
      "-feature",
      "-unchecked",
      "-Xfatal-warnings",
      "-language:postfixOps",
      s"-Xplugin:$scalacPlugin"
    )
    val javacOptions = ISZ[String](
      "-source", "1.8",
      "-target", "1.8",
      "-encoding", "utf8",
      "-XDignore.symbol.file",
      "-Xlint:-options"
    )

    def compileModule(pair: (Module, B)): (CompileStatus.Type, String) = {

      @strictpure def isJavaOrScala(p: Os.Path): B = p.ext == "scala" || p.ext == "java"

      def findSources(p: Os.Path): ISZ[Os.Path] = {
        return if (p.exists) for (p <- Os.Path.walk(p, F, followSymLink, isJavaOrScala _)) yield p else ISZ()
      }

      @pure def fingerprint(p: Os.Path): String = {
        if (sha3) {
          val sha = crypto.SHA3.init256
          sha.update(p.readU8s)
          return st"${sha.finalise()}".render
        } else {
          return s"${p.lastModified}}"
        }
      }

      val (m, forceCompile) = pair

      val base: String = m.subPathOpt match {
        case Some(p) => s"${m.basePath}$p"
        case _ => m.basePath
      }
      var sourceFiles = ISZ[Os.Path]()
      var testSourceFiles = ISZ[Os.Path]()
      for (source <- m.sources) {
        sourceFiles = sourceFiles ++ findSources(Os.path(s"$base$source"))
      }
      for (testSource <- m.testSources) {
        testSourceFiles = testSourceFiles ++ findSources(Os.path(s"$base$testSource"))
      }

      val fileTimestampMap = HashMap.empty[String, String] ++ (
        if (par && sha3) ops.ISZOps(sourceFiles ++ testSourceFiles).
          mParMap((p: Os.Path) => (path.relativize(p).string, fingerprint(p)))
        else (for (p <- sourceFiles ++ testSourceFiles) yield (path.relativize(p).string, fingerprint(p)))
        )

      val fileTimestampCache = projectOutDir / s"${m.id}${if (sha3) ".sha3" else ""}.json"

      val compile: B = if (!forceCompile && fileTimestampCache.exists) {
        val jsonParser = Json.Parser.create(fileTimestampCache.read)
        val map = jsonParser.parseHashMap(jsonParser.parseString _, jsonParser.parseString _)
        if (jsonParser.errorOpt.isEmpty) map != fileTimestampMap else T
      } else {
        T
      }

      def compileModuleH(): (CompileStatus.Type, String) = {
        var classpath: ISZ[Os.Path] = (for (lib <- dm.fetchTransitiveLibs(m)) yield Os.path(lib.main))
        classpath = classpath ++ (
          for (mDep <- dm.computeTransitiveDeps(m) if (projectOutDir / mDep / mainOutDirName).exists) yield
            projectOutDir / mDep / mainOutDirName
          )

        val mainOutDir = projectOutDir / m.id / mainOutDirName
        classpath = mainOutDir +: classpath
        mainOutDir.removeAll()
        mainOutDir.mkdirAll()
        val (mainOk, mainOut) = runCompilers(
          mid = m.id,
          category = "main",
          javaHome = javaHome,
          scalaHome = scalaHome,
          scalacOptions = scalacOptions,
          javacOptions = javacOptions,
          classpath = classpath,
          sourceFiles = sourceFiles,
          outDir = mainOutDir
        )

        if (mainOk) {
          if (testSourceFiles.nonEmpty) {
            val testOutDir = projectOutDir / m.id / testOutDirName

            classpath = classpath ++ (
              for (mDep <- dm.computeTransitiveDeps(m) if (projectOutDir / mDep / testOutDirName).exists) yield
                projectOutDir / mDep / testOutDirName
              )
            classpath = testOutDir +: classpath
            testOutDir.removeAll()
            testOutDir.mkdirAll()

            val (testOk, testOut) = runCompilers(
              mid = m.id,
              category = "test",
              javaHome = javaHome,
              scalaHome = scalaHome,
              scalacOptions = scalacOptions,
              javacOptions = javacOptions,
              classpath = classpath,
              sourceFiles = testSourceFiles,
              outDir = testOutDir
            )
            if (testOk) {
              return (CompileStatus.Compiled, s"$mainOut$testOut")
            } else {
              return (CompileStatus.Error, s"$mainOut$testOut")
            }
          } else {
            return (CompileStatus.Compiled, mainOut)
          }
        } else {
          return (CompileStatus.Error, mainOut)
        }
      }

      if (compile) {
        val r = compileModuleH()
        if (r._1 != CompileStatus.Error) {
          fileTimestampCache.writeOver(Json.Printer.printHashMap(F, fileTimestampMap, Json.Printer.printString _,
            Json.Printer.printString _).render)
        }
        return r
      } else {
        return (CompileStatus.Skipped, "")
      }

    }

    if (fresh) {
      projectOutDir.removeAll()
    }
    projectOutDir.mkdirAll()

    var modules: ISZ[(String, B)] = for (n <- project.poset.rootNodes) yield (n, compileAll)
    var compiledModuleIds = HashSet.empty[String]
    while (modules.nonEmpty) {
      var nexts = ISZ[(Module, B)]()
      var newModules = HashSMap.empty[String, B]
      for (p <- modules) {
        val m = dm.getModule(p._1)
        if (ops.ISZOps(m.deps).forall((mDep: String) => compiledModuleIds.contains(mDep))) {
          nexts = nexts :+ ((m, p._2))
        } else {
          newModules = newModules + p
        }
      }
      val nextIds: ISZ[String] = for (next <- nexts) yield next._1.id
      println(st"Compiling module${if (nextIds.size > 1) "s" else ""}: ${(nextIds, ", ")} ...".render)
      val r: ISZ[(Proyek.CompileStatus.Type, String)] =
        if (par) ops.ISZOps(nexts).mParMap(compileModule _)
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
      modules = newModules.entries
    }
    return 0
  }

  def ive(path: Os.Path,
          project: Project,
          projectName: String,
          dm: DependencyManager,
          outDirName: String,
          scalacPlugin: Os.Path,
          scalaVersion: String,
          scalaHome: Os.Path,
          sireumJar: Os.Path,
          javaHome: Os.Path,
          javaVersion: String,
          jbrVersion: String,
          ideaDir: Os.Path,
          isDev: B,
          force: B): Z = {

    val dotIdea = path / ".idea"
    dotIdea.mkdirAll()

    def writeLibraries(): Unit = {
      val ideaLib = dotIdea / "libraries"
      ideaLib.mkdirAll()

      def writeLibrary(lib: Lib): Unit = {
        val f = ideaLib / s"${lib.name}.xml"
        val javadocOpt: Option[ST] = lib.javadocOpt match {
          case Some(p) => Some(
            st"""<JAVADOC>
                |  <root url="jar://$$USER_HOME$$/${relUri(Os.home, Os.path(p))}!/" />
                |</JAVADOC>"""
          )
          case _ => None()
        }
        val sourcesOpt: Option[ST] = lib.sourcesOpt match {
          case Some(p) => Some(
            st"""<SOURCES>
                |  <root url="jar://$$USER_HOME$$/${relUri(Os.home, Os.path(p))}!/" />
                |</SOURCES>"""
          )
          case _ => None()
        }
        val st =
          st"""<component name="libraryTable">
              |  <library name="${lib.name}">
              |    <CLASSES>
              |      <root url="jar://$$USER_HOME$$/${relUri(Os.home, Os.path(lib.main))}!/" />
              |    </CLASSES>
              |    $javadocOpt
              |    $sourcesOpt
              |  </library>
              |</component>
              |"""
        f.writeOver(st.render)
        println(s"Wrote $f")
      }

      for (lib <- dm.libMap.values) {
        writeLibrary(lib)
      };
      {
        val f = ideaLib / "Sireum.xml"
        f.writeOver(
          st"""<component name="libraryTable">
              |  <library name="Sireum">
              |    <CLASSES>
              |      <root url="jar://$$USER_HOME$$/${relUri(Os.home, sireumJar)}!/" />
              |    </CLASSES>
              |    <JAVADOC />
              |    <SOURCES />
              |  </library>
              |</component>
              |""".render
        )
        println(s"Wrote $f")
      };
      {
        val scalaLibrary = relUri(Os.home, scalaHome / "lib" / "scala-library.jar")
        val scalaCompiler = relUri(Os.home, scalaHome / "lib" / "scala-compiler.jar")
        val scalaReflect = relUri(Os.home, scalaHome / "lib" / "scala-reflect.jar")

        val f = ideaLib / "Scala.xml"
        f.writeOver(
          st"""<component name="libraryTable">
              |  <library name="Scala" type="Scala">
              |    <properties>
              |      <language-level>Scala_2_13</language-level>
              |      <compiler-classpath>
              |        <root url="file://$$USER_HOME$$/$scalaLibrary" />
              |        <root url="file://$$USER_HOME$$/$scalaCompiler" />
              |        <root url="file://$$USER_HOME$$/$scalaReflect" />
              |      </compiler-classpath>
              |    </properties>
              |    <CLASSES>
              |      <root url="jar://$$USER_HOME$$/$scalaLibrary!/" />
              |      <root url="jar://$$USER_HOME$$/$scalaReflect!/" />
              |    </CLASSES>
              |    <JAVADOC>
              |      <root url="https://www.scala-lang.org/api/$scalaVersion/" />
              |    </JAVADOC>
              |    <SOURCES />
              |  </library>
              |</component>
              |""".render
        )
        println(s"Wrote $f")
      }
    }

    def writeModules(): Unit = {
      val dotIdeaModules = path / ".idea_modules"
      dotIdeaModules.mkdirAll()

      var moduleEntries = ISZ[ST]()

      def writeModule(m: Module): Unit = {
        moduleEntries = moduleEntries :+
          st"""<module fileurl="file://$$PROJECT_DIR$$/.idea_modules/${m.id}.iml" filepath="$$PROJECT_DIR$$${Os.fileSep}.idea_modules${Os.fileSep}${m.id}.iml" />"""
        val basePath: String = m.subPathOpt match {
          case Some(subPath) => s"${m.basePath}$subPath"
          case _ => m.basePath
        }
        val deps: ISZ[ST] = for (dep <- m.deps) yield
          st"""<orderEntry type="module" module-name="$dep" exported="" />"""
        val sources: ISZ[ST] = for (src <- m.sources) yield
          st"""<sourceFolder url="file://$$MODULE_DIR$$/${relUri(dotIdeaModules, Os.path(s"$basePath$src"))}" isTestSource="false" />"""
        val resources: ISZ[ST] = for (rsc <- m.resources) yield
          st"""<sourceFolder url="file://$$MODULE_DIR$$/${relUri(dotIdeaModules, Os.path(s"$basePath$rsc"))}" type="java-resource" />"""
        val testSources: ISZ[ST] = for (src <- m.testSources) yield
          st"""<sourceFolder url="file://$$MODULE_DIR$$/${relUri(dotIdeaModules, Os.path(s"$basePath$src"))}" isTestSource="true" />"""
        val testResources: ISZ[ST] = for (rsc <- m.testResources) yield
          st"""<sourceFolder url="file://$$MODULE_DIR$$/${relUri(dotIdeaModules, Os.path(s"$basePath$rsc"))}" type="java-test-resource" />"""
        val libs: ISZ[ST] = for (lib <- dm.fetchDiffLibs(m)) yield
          st"""<orderEntry type="library" name="${lib.name}" level="project" exported="" />"""
        val st =
          st"""<?xml version="1.0" encoding="UTF-8"?>
              |<module type="JAVA_MODULE" version="4">
              |  <component name="NewModuleRootManager" inherit-compiler-output="true">
              |    <exclude-output />
              |    <content url="file://$$MODULE_DIR$$/${relUri(dotIdeaModules, Os.path(basePath))}">
              |      ${(sources, "\n")}
              |      ${(resources, "\n")}
              |      ${(testSources, "\n")}
              |      ${(testResources, "\n")}
              |    </content>
              |    <orderEntry type="sourceFolder" forTests="false" />
              |    ${(deps, "\n")}
              |    ${(libs, "\n")}
              |    <orderEntry type="library" name="Scala" level="project" />
              |    <orderEntry type="inheritedJdk" />
              |  </component>
              |</module>
              |"""
        val f = dotIdeaModules / s"${m.id}.iml"
        f.writeOver(st.render)
        println(s"Wrote $f")
      }

      var moduleIds = project.poset.rootNodes
      while (moduleIds.nonEmpty) {
        var newModuleIds = HashSSet.empty[String]
        for (mid <- moduleIds) {
          val module: Module = project.modules.get(mid) match {
            case Some(m) => m
            case _ => halt(s"Could not find module with id '$mid' in the project")
          }
          writeModule(module)
          newModuleIds = newModuleIds ++ project.poset.childrenOf(mid).elements
        }
        moduleIds = newModuleIds.elements
      };
      {
        val f = dotIdeaModules / s"$projectName.iml"
        f.writeOver(
          st"""<?xml version="1.0" encoding="UTF-8"?>
              |<module type="JAVA_MODULE" version="4">
              |  <component name="NewModuleRootManager" inherit-compiler-output="true">
              |    <exclude-output />
              |    <content url="file://$$MODULE_DIR$$/.." />
              |    <orderEntry type="inheritedJdk" />
              |    <orderEntry type="library" name="Scala" level="project" />
              |    <orderEntry type="library" name="Sireum" level="project" />
              |    <orderEntry type="sourceFolder" forTests="false" />
              |  </component>
              |</module>
              |""".render
        )
        println(s"Wrote $f")
      }

      moduleEntries = moduleEntries :+
        st"""<module fileurl="file://$$PROJECT_DIR$$/.idea_modules/$projectName.iml" filepath="$$PROJECT_DIR$$${Os.fileSep}.idea_modules${Os.fileSep}$projectName.iml" />""";
      {
        val f = dotIdea / "modules.xml"
        f.writeOver(
          st"""<?xml version="1.0" encoding="UTF-8"?>
              |<project version="4">
              |  <component name="ProjectModuleManager">
              |    <modules>
              |      ${(moduleEntries, "\n")}
              |    </modules>
              |  </component>
              |</project>
              |""".render
        )
        println(s"Wrote $f")
      }
    }

    writeLibraries()
    writeModules()
    IVE.writeMisc(dotIdea, outDirName)
    IVE.writeCompiler(dotIdea)
    IVE.writeScalaCompiler(dotIdea, scalacPlugin)
    IVE.writeScalaSettings(dotIdea)
    IVE.writeInspectionProfiles(dotIdea)
    IVE.writeUiDesigner(dotIdea)
    IVE.writeScriptRunner(dotIdea, projectName)
    IVE.writeApplicationConfigs(force, ideaDir, javaHome, javaVersion, jbrVersion, if (isDev) "" else "-dev")

    return 0
  }

  def publish(path: Os.Path,
              outDirName: String,
              project: Project,
              projectName: String,
              dm: DependencyManager,
              orgName: ISZ[String],
              m2Repo: Os.Path,
              version: String,
              scalaMajorVersion: String,
              symlink: B): Z = {

    @strictpure def shouldCopy(p: Os.Path): B = !ignoredPathNames.contains(p.name)

    val m2Base = m2Repo /+ orgName
    m2Base.mkdirAll()

    val proyekDir = getProyekDir(path, outDirName, projectName)

    val projectOutDir = proyekDir / "modules"

    for (m <- project.modules.values if m.publishInfoOpt.nonEmpty) {

      val mOutDir = projectOutDir / m.id

      val org = st"${(orgName, ".")}".render
      val module = s"${m.id}_$scalaMajorVersion"

      var base = m.basePath
      m.subPathOpt match {
        case Some(subPath) => base = s"$base$subPath"
        case _ =>
      }

      def writeMainJar(): Unit = {
        val mOutMainDir = mOutDir / mainOutDirName

        for (resource <- m.resources) {
          val resourcePath = Os.path(s"$base$resource")
          resourcePath.overlayCopy(mOutMainDir, F, symlink, shouldCopy _, F)
        }

        val mainMetaInf = mOutMainDir / metaInf / manifestMf
        mainMetaInf.up.mkdirAll()
        mainMetaInf.writeOver(
          st"""Manifest-Version: 1.0
              |Created-By: Sireum Proyek
              |""".render
        )

        val m2MainJar = m2Base / module / version / s"$module-$version.jar"
        m2MainJar.up.mkdirAll()
        mOutMainDir.zipTo(m2MainJar)
        println(s"Wrote $m2MainJar")

        val m2MainJarSha1 = (m2MainJar.up / s"${m2MainJar.name}.sha1").canon
        m2MainJarSha1.writeOver(m2MainJar.sha1)
        println(s"Wrote $m2MainJarSha1")

        val m2MainJarMd5 = (m2MainJar.up / s"${m2MainJar.name}.md5").canon
        m2MainJarMd5.writeOver(m2MainJar.md5)
        println(s"Wrote $m2MainJarMd5")
      }

      def writeSourcesJar(): Unit = {
        val mOutSourcesDir = mOutDir / sourcesOutDirName

        for (source <- m.sources) {
          val sourcePath = Os.path(s"$base$source")
          sourcePath.overlayCopy(mOutSourcesDir, F, symlink, shouldCopy _, F)
        }

        for (resource <- m.resources) {
          val resourcePath = Os.path(s"$base$resource")
          resourcePath.overlayCopy(mOutSourcesDir, F, symlink, shouldCopy _, F)
        }

        val sourcesMetaInf = mOutSourcesDir / metaInf / manifestMf
        sourcesMetaInf.up.mkdirAll()
        sourcesMetaInf.writeOver(
          st"""Manifest-Version: 1.0
              |Created-By: Sireum Proyek
              |""".render
        )

        val m2SourcesJar = m2Base / module / version / s"$module-$version-sources.jar"
        m2SourcesJar.up.mkdirAll()
        mOutSourcesDir.zipTo(m2SourcesJar)
        println(s"Wrote $m2SourcesJar")

        val m2SourcesJarSha1 = (m2SourcesJar.up / s"${m2SourcesJar.name}.sha1").canon
        m2SourcesJarSha1.writeOver(m2SourcesJar.sha1)
        println(s"Wrote $m2SourcesJarSha1")

        val m2SourcesJarMd5 = (m2SourcesJar.up / s"${m2SourcesJar.name}.md5").canon
        m2SourcesJarMd5.writeOver(m2SourcesJar.md5)
        println(s"Wrote $m2SourcesJarMd5")
      }

      def writePom(): Unit = {
        var deps = ISZ[ST]()

        for (mDep <- m.deps) {
          deps = deps :+ PomTemplate.dep(org, s"${mDep}_$scalaMajorVersion", version)
        }

        for (ivyDep <- m.ivyDeps) {
          val cif = Coursier.fetch(ISZ(dm.ivyDeps.get(ivyDep).get))(0)
          deps = deps :+ PomTemplate.dep(cif.org, cif.module, cif.version)
        }

        val m2Pom = m2Base / module / version / s"$module-$version.pom"
        m2Pom.up.mkdirAll()

        val pi = m.publishInfoOpt.get

        m2Pom.writeOver(
          PomTemplate.pom(
            name = st"$org.${m.id}_$scalaMajorVersion".render,
            org = org,
            module = module,
            description = pi.description,
            version = version,
            url = pi.url,
            licenses = for (l <- pi.licenses) yield PomTemplate.license(l.name, l.url, l.distribution),
            developers = for (d <- pi.developers) yield PomTemplate.dev(d.id, d.name),
            dependencies = deps
          ).render
        )
        println(s"Wrote $m2Pom")

        val m2PomSha1 = (m2Pom.up / s"${m2Pom.name}.sha1").canon
        m2PomSha1.writeOver(m2Pom.sha1)
        println(s"Wrote $m2PomSha1")

        val m2PomMd5 = (m2Pom.up / s"${m2Pom.name}.md5").canon
        m2PomMd5.writeOver(m2Pom.md5)
        println(s"Wrote $m2PomMd5")
      }

      writeMainJar()
      writeSourcesJar()
      writePom()
    }

    return 0
  }

  def test(path: Os.Path,
           outDirName: String,
           project: Project,
           projectName: String,
           dm: DependencyManager,
           javaHome: Os.Path,
           classNames: ISZ[String],
           names: ISZ[String]): Z = {

    val proyekDir = getProyekDir(path, outDirName, projectName)
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

      var base = m.basePath
      m.subPathOpt match {
        case Some(subPath) => base = s"$base$subPath"
        case _ =>
      }
      testClasspath = testClasspath ++ (for (r <- m.resources ++ m.testResources) yield s"$base$r")
    }

    for (lib <- dm.libMap.values) {
      testClasspath = testClasspath :+ lib.main
    }

    val classpath: ISZ[String] =
      for (cif <- Coursier.fetch(ISZ(s"org.scalatest::scalatest::${dm.versions.get("org.scalatest%%scalatest%%").get}"))) yield cif.path.string

    var args = ISZ[String](
      "-classpath", st"${(classpath, Os.pathSep)}".render,
      "org.scalatest.tools.Runner",
      "-oF", "-P1",
      "-R", st""""${(if (Os.isWin) for (p <- testClasspath) yield ops.StringOps(p).replaceAllLiterally("\\", "\\\\")
        else testClasspath, " ")}"""".render
    )
    args = args ++ (for (args2 <- for (name <- classNames) yield ISZ[String]("-s", name); arg <- args2) yield arg)
    args = args ++ (for (args2 <- for (name <- names) yield ISZ[String]("-w", name); arg <- args2) yield arg)

    val argFile = proyekDir / "java-test-args"
    argFile.writeOver(st"${(args, "\n")}".render)

    val javaExe = javaHome / "bin" / (if (Os.isWin) "java.exe" else "java")
    proc"$javaExe @$argFile".at(path).console.runCheck()

    return 0
  }


  @strictpure def getProyekDir(path: Os.Path, outDirName: String, projectName: String): Os.Path =
    path / outDirName / s"$projectName"

  @strictpure def libName(cif: CoursierFileInfo): String = s"${cif.org}.${cif.module}"

  @pure def buildClassifiers(withSource: B, withDoc: B): ISZ[CoursierClassifier.Type] = {
    var classifiers = ISZ[CoursierClassifier.Type](CoursierClassifier.Default)
    if (withSource) {
      classifiers = classifiers :+ CoursierClassifier.Sources
    }
    if (withDoc) {
      classifiers = classifiers :+ CoursierClassifier.Javadoc
    }
    return classifiers
  }

  @pure def normalizePath(path: String): String = {
    if (Os.isWin) {
      return path
    } else {
      return ops.StringOps(path).replaceAllChars('\\', '/')
    }
  }

  @strictpure def relUri(from: Os.Path, to: Os.Path): String = normalizePath(from.relativize(to).string)

  object IVE {

    def writeApplicationConfigs(force: B,
                                ideaDir:
                                Os.Path,
                                javaHome: Os.Path,
                                javaVersion: String,
                                jbrVersion: String,
                                devSuffix: String): Unit = {

      val configOptions: Os.Path =
        if (Os.isMac) Os.home / "Library" / "Application Support" / "JetBrains" / s"SireumIVE$devSuffix" / "options"
        else Os.home / s".SireumIVE$devSuffix" / "config" / "options"
      val configColors = (configOptions.up / "colors").canon
      configOptions.mkdirAll()
      configColors.mkdirAll()

      def writeJdkTable(): Unit = {

        val jdkTableXml = configOptions / "jdk.table.xml"
        if (!force && jdkTableXml.exists) {
          return
        }

        val jbrHome: Os.Path = if (Os.isMac) ideaDir / "jbr" / "Contents" / "Home" else ideaDir / "jbr"

        val jdkModules: Set[String] = Set ++ (for (p <- (javaHome / "jmods").list if p.ext === "jmod") yield
          ops.StringOps(p.name).substring(0, p.name.size - 5)
          )

        val jbrModules: Set[String] = Set ++ ISZ(
          "gluegen.rt", "java.base", "java.compiler", "java.datatransfer", "java.desktop", "java.instrument",
          "java.logging", "java.management", "java.management.rmi", "java.naming", "java.net.http", "java.prefs",
          "java.rmi", "java.scripting", "java.se", "java.security.jgss", "java.security.sasl", "java.smartcardio",
          "java.sql", "java.sql.rowset", "java.transaction.xa", "java.xml", "java.xml.crypto", "jcef",
          "jdk.accessibility", "jdk.aot", "jdk.attach", "jdk.charsets", "jdk.compiler", "jdk.crypto.cryptoki",
          "jdk.crypto.ec", "jdk.dynalink", "jdk.hotspot.agent", "jdk.httpserver", "jdk.internal.ed",
          "jdk.internal.jvmstat", "jdk.internal.le", "jdk.internal.vm.ci", "jdk.internal.vm.compiler",
          "jdk.internal.vm.compiler.management", "jdk.jdi", "jdk.jdwp.agent", "jdk.jfr", "jdk.jsobject",
          "jdk.localedata", "jdk.management", "jdk.management.agent", "jdk.management.jfr", "jdk.naming.dns",
          "jdk.naming.rmi", "jdk.net", "jdk.pack", "jdk.scripting.nashorn", "jdk.scripting.nashorn.shell",
          "jdk.sctp", "jdk.security.auth", "jdk.security.jgss", "jdk.unsupported", "jdk.xml.dom", "jdk.zipfs",
          "jogl.all"
        )

        val ideaLibDir = ideaDir / "lib"
        val ideaPluginsDir = ideaDir / "plugins"

        val (jdkClassPath, jdkSourcePath): (ISZ[ST], ISZ[ST]) =
          (for (m <- jdkModules.elements) yield
            st"""            <root url="jrt://${normalizePath(javaHome.string)}!/$m" type="simple" />""",
            for (m <- jdkModules.elements) yield
              st"""            <root url="jar://${normalizePath(javaHome.string)}/lib/src.zip!/$m" type="simple" />""")
        val jbrClassPath: ISZ[ST] = for (m <- jbrModules.elements) yield
          st"""            <root url="jrt://${normalizePath(jbrHome.string)}!/$m" type="simple" />"""
        val ideaLibs: ISZ[ST] = for (p <- Os.Path.walk(ideaLibDir, F, T, f => ops.StringOps(f.string).endsWith(".jar")))
          yield st"""            <root url="jar://${normalizePath(p.string)}!/" type="simple" />"""
        val ideaJavaLibs: ISZ[ST] = for (p <- Os.Path.walk(ideaPluginsDir / "java" / "lib", F, T, f => ops.StringOps(f.string).endsWith(".jar")))
          yield st"""            <root url="jar://${normalizePath(p.string)}!/" type="simple" />"""
        val ideaScalaLibs: ISZ[ST] = for (p <- Os.Path.walk(ideaPluginsDir / "Scala" / "lib", F, T, f => ops.StringOps(f.string).endsWith(".jar")))
          yield st"""            <root url="jar://${normalizePath(p.string)}!/" type="simple" />"""

        val idea =
          st"""    <jdk version="2">
              |      <name value="Sireum$devSuffix" />
              |      <type value="IDEA JDK" />
              |      <version value="$jbrVersion" />
              |      <homePath value="$ideaDir" />
              |      <roots>
              |        <annotationsPath>
              |          <root type="composite">
              |            <root url="jar://$$APPLICATION_HOME_DIR$$/plugins/java/lib/jdkAnnotations.jar!/" type="simple" />
              |          </root>
              |        </annotationsPath>
              |        <classPath>
              |          <root type="composite">
              |${(jbrClassPath, "\n")}
              |${(ideaLibs, "\n")}
              |          </root>
              |        </classPath>
              |        <javadocPath>
              |          <root type="composite">
              |            <root url="https://docs.oracle.com/en/java/javase/11/docs/api/" type="simple" />
              |          </root>
              |        </javadocPath>
              |      </roots>
              |      <additional sdk="Jbr">
              |        <option name="mySandboxHome" value="$$USER_HOME$$/.SireumIVE$devSuffix-sandbox" />
              |      </additional>
              |    </jdk>"""


        val ideaScala =
          st"""    <jdk version="2">
              |      <name value="Sireum$devSuffix (with Scala Plugin)" />
              |      <type value="IDEA JDK" />
              |      <version value="$jbrVersion" />
              |      <homePath value="$ideaDir" />
              |      <roots>
              |        <annotationsPath>
              |          <root type="composite" />
              |        </annotationsPath>
              |        <classPath>
              |          <root type="composite">
              |${(jbrClassPath, "\n")}
              |${(ideaLibs, "\n")}
              |${(ideaJavaLibs, "\n")}
              |${(ideaScalaLibs, "\n")}
              |          </root>
              |        </classPath>
              |        <javadocPath>
              |          <root type="composite">
              |            <root url="https://docs.oracle.com/en/java/javase/11/docs/api/" type="simple" />
              |          </root>
              |        </javadocPath>
              |      </roots>
              |      <additional sdk="Jbr">
              |        <option name="mySandboxHome" value="$$USER_HOME$$/.SireumIVE$devSuffix-sandbox" />
              |      </additional>
              |    </jdk>"""

        val table =
          st"""<application>
              |  <component name="ProjectJdkTable">
              |    <jdk version="2">
              |      <name value="Java" />
              |      <type value="JavaSDK" />
              |      <version value="$javaVersion" />
              |      <homePath value="$javaHome" />
              |      <roots>
              |        <annotationsPath>
              |          <root type="composite">
              |            <root url="jar://$$APPLICATION_HOME_DIR$$/lib/jdkAnnotations.jar!/" type="simple" />
              |          </root>
              |        </annotationsPath>
              |        <classPath>
              |          <root type="composite">
              |${(jdkClassPath, "\n")}
              |          </root>
              |        </classPath>
              |        <javadocPath>
              |          <root type="composite">
              |            <root url="https://docs.oracle.com/en/java/javase/16/docs/api/" type="simple" />
              |          </root>
              |        </javadocPath>
              |        <sourcePath>
              |          <root type="composite">
              |${(jdkSourcePath, "\n")}
              |          </root>
              |        </sourcePath>
              |      </roots>
              |      <additional />
              |    </jdk>
              |    <jdk version="2">
              |      <name value="Jbr" />
              |      <type value="JavaSDK" />
              |      <version value="$jbrVersion" />
              |      <homePath value="$jbrHome" />
              |      <roots>
              |        <annotationsPath>
              |          <root type="composite">
              |            <root url="jar://$$APPLICATION_HOME_DIR$$/lib/jdkAnnotations.jar!/" type="simple" />
              |          </root>
              |        </annotationsPath>
              |        <classPath>
              |          <root type="composite">
              |${(jbrClassPath, "\n")}
              |          </root>
              |        </classPath>
              |        <javadocPath>
              |          <root type="composite">
              |            <root url="https://docs.oracle.com/en/java/javase/11/docs/api/" type="simple" />
              |          </root>
              |        </javadocPath>
              |        <sourcePath>
              |          <root type="composite" />
              |        </sourcePath>
              |      </roots>
              |      <additional />
              |    </jdk>
              |$idea
              |$ideaScala
              |  </component>
              |</application>"""

        jdkTableXml.writeOver(table.render)
        println(s"Wrote $jdkTableXml")
      }

      def writeFileTypes(): Unit = {
        val fileTypesXml = configOptions / "filetypes.xml"
        if (!force && fileTypesXml.exists) {
          return
        }
        fileTypesXml.writeOver(
          st"""<application>
              |  <component name="FileTypeManager" version="17">
              |    <extensionMap>
              |      <mapping ext="cmd" type="Scala" />
              |      <removed_mapping ext="cmd" approved="true" type="PLAIN_TEXT" />
              |    </extensionMap>
              |  </component>
              |</application>""".render
        )
        println(s"Wrote $fileTypesXml")
      }

      def writeColors(): Unit = {
        for (name <- ISZ[String]("Darcula", "Default")) {
          val f = configColors / s"_@user_$name.icls"
          if (!force && f.exists) {
            return
          }
          f.writeOver(
            st"""<scheme name="_@user_$name" version="142" parent_scheme="$name">
                |  <attributes>
                |    <option name="DEPRECATED_ATTRIBUTES">
                |      <value>
                |        <option name="EFFECT_TYPE" value="3" />
                |      </value>
                |    </option>
                |  </attributes>
                |</scheme>
                |""".render
          )
          println(s"Wrote $f")
        }
      }

      writeJdkTable()
      writeFileTypes()
      writeColors()

      (Os.home / s".SireumIVE$devSuffix-sandbox").mkdirAll()
    }

    def writeMisc(dotIdea: Os.Path, outDirName: String): Unit = {
      val f = dotIdea / "misc.xml"
      f.writeOver(
        st"""<?xml version="1.0" encoding="UTF-8"?>
            |<project version="4">
            |  <component name="ProjectRootManager" version="2" languageLevel="JDK_1_8" project-jdk-name="Java" project-jdk-type="JavaSDK">
            |    <output url="file://$$PROJECT_DIR$$/$outDirName" />
            |  </component>
            |</project>
            |""".render
      )
      println(s"Wrote $f")
    }

    def writeCompiler(dotIdea: Os.Path): Unit = {
      val f = dotIdea / "compiler.xml"
      f.writeOver(
        st"""<?xml version="1.0" encoding="UTF-8"?>
            |<project version="4">
            |  <component name="CompilerConfiguration">
            |    <option name="USE_RELEASE_OPTION" value="false" />
            |  </component>
            |</project>
            |""".render
      )
      println(s"Wrote $f")
    }

    def writeScalaCompiler(dotIdea: Os.Path, scalacPlugin: Os.Path): Unit = {
      val f = dotIdea / "scala_compiler.xml"
      f.writeOver(
        st"""<?xml version="1.0" encoding="UTF-8"?>
            |<project version="4">
            |  <component name="ScalaCompilerConfiguration">
            |    <option name="postfixOps" value="true" />
            |    <option name="deprecationWarnings" value="true" />
            |    <option name="uncheckedWarnings" value="true" />
            |    <option name="featureWarnings" value="true" />
            |    <parameters>
            |      <parameter value="-target:jvm-1.8" />
            |      <parameter value="-Yrangepos" />
            |      <parameter value="-Ydelambdafy:method" />
            |      <parameter value="-Xfatal-warnings" />
            |    </parameters>
            |    <plugins>
            |      <plugin path="$$USER_HOME$$${Os.fileSep}${Os.home.relativize(scalacPlugin)}" />
            |    </plugins>
            |  </component>
            |</project>
            |""".render
      )
      println(s"Wrote $f")
    }

    def writeScalaSettings(dotIdea: Os.Path): Unit = {
      val f = dotIdea / "scala_settings.xml"
      f.writeOver(
        st"""<?xml version="1.0" encoding="UTF-8"?>
            |<project version="4">
            |  <component name="ScalaProjectSettings">
            |    <option name="autoRunDelay" value="3000" />
            |    <option name="dontCacheCompoundTypes" value="true" />
            |    <option name="inProcessMode" value="false" />
            |    <option name="intInjectionMapping">
            |      <map>
            |        <entry key="xml" value="XML" />
            |      </map>
            |    </option>
            |    <option name="metaTrimMethodBodies" value="false" />
            |    <option name="scFileMode" value="Ammonite" />
            |    <option name="scalaMetaMode" value="Disabled" />
            |    <option name="showNotFoundImplicitArguments" value="false" />
            |    <option name="treatDocCommentAsBlockComment" value="true" />
            |    <option name="treatScratchFilesAsWorksheet" value="false" />
            |  </component>
            |</project>""".render
      )
      println(s"Wrote $f")
    }

    def writeInspectionProfiles(dotIdea: Os.Path): Unit = {
      val inspectionProfiles = dotIdea / "inspectionProfiles"
      inspectionProfiles.mkdirAll()
      val f = inspectionProfiles / "Project_Default.xml"
      f.writeOver(
        st"""<component name="InspectionProjectProfileManager">
            |  <profile version="1.0">
            |    <option name="myName" value="Project Default" />
            |    <inspection_tool class="ComparingUnrelatedTypes" enabled="false" level="WARNING" enabled_by_default="false" />
            |    <inspection_tool class="ConvertibleToMethodValue" enabled="false" level="WARNING" enabled_by_default="false" />
            |    <inspection_tool class="RemoveRedundantReturn" enabled="false" level="WARNING" enabled_by_default="false" />
            |  </profile>
            |</component>
            |""".render
      )
      println(s"Wrote $f")
    }

    def writeUiDesigner(dotIdea: Os.Path): Unit = {
      val f = dotIdea / "uiDesigner.xml"
      f.writeOver(
        st"""<?xml version="1.0" encoding="UTF-8"?>
            |<project version="4">
            |  <component name="uidesigner-configuration">
            |    <option name="INSTRUMENT_CLASSES" value="true" />
            |  </component>
            |</project>
            |""".render
      )
      println(s"Wrote $f")
    }

    def writeScriptRunner(dotIdea: Os.Path, name: String): Unit = {
      val runConfigurations = dotIdea / "runConfigurations"
      runConfigurations.mkdirAll()

      val f = runConfigurations / "Slang_Script_Runner.xml"
      f.writeOver(
        st"""<component name="ProjectRunConfigurationManager">
            |  <configuration default="false" name="Slang Script Runner" type="Application" factoryName="Application" singleton="false">
            |    <option name="MAIN_CLASS_NAME" value="org.sireum.Sireum" />
            |    <module name="$name" />
            |    <option name="PROGRAM_PARAMETERS" value="slang run $$FilePath$$" />
            |    <method v="2" />
            |  </configuration>
            |</component>
            |""".render
      )
      println(s"Wrote $f")
    }

  }

  object PomTemplate {

    @strictpure def license(name: String, url: String, distribution: String): ST =
      st"""<license>
          |  <name>$name</name>
          |  <url>$url</url>
          |  <distribution>$distribution</distribution>
          |</license>"""

    @strictpure def dev(id: String, name: String): ST =
      st"""<developer>
          |  <id>$id</id>
          |  <name>$name</name>
          |</developer>"""

    @strictpure def dep(org: String, module: String, version: String): ST =
      st"""<dependency>
          |  <groupId>$org</groupId>
          |  <artifactId>$module</artifactId>
          |  <version>$version</version>
          |</dependency>"""

    @strictpure def pom(name: String,
                        org: String,
                        module: String,
                        description: String,
                        version: String,
                        url: String,
                        licenses: ISZ[ST],
                        developers: ISZ[ST],
                        dependencies: ISZ[ST]): ST =
      st"""<?xml version="1.0" encoding="UTF-8"?>
          |<project xsi:schemaLocation="http://maven.apache.org/POM/4.0.0 http://maven.apache.org/xsd/maven-4.0.0.xsd"
          |         xmlns="http://maven.apache.org/POM/4.0.0"
          |         xmlns:xsi="http://www.w3.org/2001/XMLSchema-instance">
          |  <modelVersion>4.0.0</modelVersion>
          |  <name>$name</name>
          |  <groupId>$org</groupId>
          |  <artifactId>$module</artifactId>
          |  <packaging>jar</packaging>
          |  <description>$description</description>
          |  <version>$version</version>
          |  <url>https://$url</url>
          |  <licenses>
          |    ${(licenses, "\n")}
          |  </licenses>
          |  <scm>
          |    <connection>scm:git://$url.git</connection>
          |    <url>git://$url.git</url>
          |  </scm>
          |  <developers>
          |    ${(developers, "\n")}
          |  </developers>
          |  <dependencies>
          |    ${(dependencies, "\n")}
          |  </dependencies>
          |</project>
          |"""
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

    val javaSources: ISZ[String] = for (f <- sourceFiles if f.ext === "java") yield f.string
    val numOfScalaFiles: Z = sourceFiles.size - javaSources.size
    val numOfJavaFiles: Z = javaSources.size

    (numOfScalaFiles, numOfJavaFiles) match {
      case (z"0", _) => sb = sb :+ st"* Compiled $numOfJavaFiles Java $mid $category source file${if (numOfJavaFiles > 1) "s" else ""}\n"
      case (_, z"0") => sb = sb :+ st"* Compiled $numOfScalaFiles Scala $mid $category source file${if (numOfScalaFiles > 1) "s" else ""}\n"
      case (_, _) => sb = sb :+ st"* Compiled $numOfScalaFiles Scala and $numOfJavaFiles Java $mid $category source files\n"
    }

    if (numOfScalaFiles > 0) {
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
        val r = proc"$javac @${argFile.name}".at(argFile.up.canon).console.run()
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

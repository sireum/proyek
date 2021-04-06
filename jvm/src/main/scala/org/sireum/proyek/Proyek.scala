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

  @ext("Proyek_Ext") object Ext {
    def compile(mid: String,
                category: String,
                javaHome: Os.Path,
                scalaHome: Os.Path,
                scalacOptions: ISZ[String],
                javacOptions: ISZ[String],
                classpath: ISZ[Os.Path],
                sourceFiles: ISZ[Os.Path],
                outDir: Os.Path): (B, String) = $

    def rewriteReleaseFence(jar: Os.Path): Unit = $

    def test(args: ISZ[String]): Unit = $
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
      versions.get(ops.StringOps(ivyDep).replaceAllChars(':', '%'))match {
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
        r = r ++ computeTransitiveDeps(getModule(mDep)) + mDep
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
  val cacheSuffix: String = "-inc-cache.zip"
  val mainOutDirName: String = "classes"
  val mainCacheName: String = s"$mainOutDirName$cacheSuffix"
  val testOutDirName: String = "test-classes"
  val testCacheName: String = s"$testOutDirName$cacheSuffix"


  def assemble(path: Os.Path,
               outDirName: String,
               project: Project,
               projectName: String,
               dm: DependencyManager,
               scalaHome: Os.Path,
               mainClassNameOpt: Option[String]): Z = {

    val trueF = (_: Os.Path) => T

    val proyekDir = getProyekDir(path, outDirName, projectName)
    val projectOutDir = proyekDir / "modules"

    val assembleDir = proyekDir / "assemble"
    val contentDir = assembleDir / "content"
    val jar = assembleDir / s"$projectName.jar"
    jar.removeAll()

    println(s"Assembling ...")

    contentDir.removeAll()
    contentDir.mkdirAll()

    val metaDir = contentDir / "META-INF"
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

    val manifest = metaDir / "MANIFEST.MF"
    val mainOpt: Option[ST] = mainClassNameOpt.map((mainClassName: String) => st"Main-Class: $mainClassName")
    manifest.writeOver(
      st"""Manifest-Version: 1.0
          |Created-By: Sireum Proyek
          |$mainOpt
          |""".render
    )

    contentDir.zipTo(jar)

    Ext.rewriteReleaseFence(jar)

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

    val compileAll: B = if (versionsCache.exists) {
      val jsonParser = Json.Parser.create(versionsCache.read)
      val m = jsonParser.parseHashSMap(jsonParser.parseString _, jsonParser.parseString _)
      if (jsonParser.errorOpt.isEmpty) m != dm.versions else T
    } else {
      T
    }
    if (compileAll) {
      versionsCache.writeOver(
        Json.Printer.printHashSMap(F, dm.versions, Json.Printer.printString _, Json.Printer.printString _).render
      )
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

      if (compile) {
        fileTimestampCache.writeOver(Json.Printer.printHashMap(F, fileTimestampMap, Json.Printer.printString _,
          Json.Printer.printString _).render)
      } else {
        return (CompileStatus.Skipped, "")
      }

      var classpath: ISZ[Os.Path] = (for (lib <- dm.fetchTransitiveLibs(m)) yield Os.path(lib.main))
      for (mDep <- dm.computeTransitiveDeps(m)) {
        val p = projectOutDir / mDep / mainOutDirName
        if (p.exists) {
          classpath = classpath :+ p
        }
      }

      val mainOutDir = projectOutDir / m.id / mainOutDirName
      classpath = classpath :+ mainOutDir
      mainOutDir.removeAll()
      mainOutDir.mkdirAll()
      val (mainOk, mainOut) = Ext.compile(
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

          for (mDep <- dm.computeTransitiveDeps(m)) {
            val p = projectOutDir / mDep / testOutDirName
            if (p.exists) {
              classpath = classpath :+ p
            }
          }
          classpath = classpath :+ testOutDir
          testOutDir.removeAll()
          testOutDir.mkdirAll()

          val (testOk, testOut) = Ext.compile(
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
      val r: ISZ[(CompileStatus.Type, String)] =
        if (par) ops.ISZOps(nexts).mParMap(compileModule _) else for (next <- nexts) yield compileModule(next)
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
      }

      ;{
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
      }

      ;{
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
              |    <orderEntry type="inheritedJdk" />
              |    <orderEntry type="library" name="Scala" level="project" />
              |    ${(deps, "\n")}
              |    <orderEntry type="sourceFolder" forTests="false" />
              |    ${(libs, "\n")}
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
      }

      ;{
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
        st"""<module fileurl="file://$$PROJECT_DIR$$/.idea_modules/$projectName.iml" filepath="$$PROJECT_DIR$$${Os.fileSep}.idea_modules${Os.fileSep}$projectName.iml" />"""

      ;{
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

  def test(path: Os.Path,
           outDirName: String,
           project: Project,
           projectName: String,
           dm: DependencyManager,
           javaHome: Os.Path,
           par: B,
           names: ISZ[String]): Z = {

    val proyekDir = getProyekDir(path, outDirName, projectName)
    val projectOutDir = proyekDir / "modules"

    val classpath: ISZ[String] =
      for (cif <- Coursier.fetch(ISZ(s"org.scalatest::scalatest::${dm.versions.get("org.scalatest%%scalatest%%").get}"))) yield cif.path.string

    var testClasspath = ISZ[String]()

    for (lib <- dm.libMap.values) {
      testClasspath = testClasspath :+ lib.main
    }

    for (m <- project.modules.values) {
      val mDir = projectOutDir / m.id / mainOutDirName
      val mTestDir = projectOutDir / m.id / testOutDirName
      testClasspath = testClasspath ++ ISZ(mDir.string, mTestDir.string)

      var base = m.basePath
      m.subPathOpt match {
        case Some(subPath) => base = s"$base$subPath"
        case _ =>
      }
      testClasspath = testClasspath ++ (for (r <- m.resources ++ m.testResources) yield s"$base$r")
    }

    var args = ISZ[String](
      "-o",
      "-R", st"""${(testClasspath, " ")}""".render
    )
    if (par) {
      args = args :+ "-P"
    }
    args = args ++ (for (args2 <- for (name <- names) yield ISZ[String]("-w", name); arg <- args2) yield arg)

    Ext.test(args)

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

      def writeJdkTable(xml: Os.Path): Unit = {

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
          ( for (m <- jdkModules.elements) yield
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

        xml.writeOver(table.render)
        println(s"Wrote $xml")
      }

      def writeFileTypes(xml: Os.Path): Unit = {
        xml.writeOver(
          st"""<application>
              |  <component name="FileTypeManager" version="17">
              |    <extensionMap>
              |      <mapping ext="cmd" type="Scala" />
              |      <removed_mapping ext="cmd" approved="true" type="PLAIN_TEXT" />
              |    </extensionMap>
              |  </component>
              |</application>""".render
        )
        println(s"Wrote $xml")
      }

      val configOptions: Os.Path =
        if (Os.isMac) Os.home / "Library" / "Application Support" / "JetBrains" / s"SireumIVE$devSuffix" / "options"
        else Os.home / s".SireumIVE$devSuffix" / "config" / "options"
      configOptions.mkdirAll()


      val jdkTableXml = configOptions / "jdk.table.xml"
      if (force || !jdkTableXml.exists) {
        writeJdkTable(jdkTableXml)
      }

      val fileTypesXml = configOptions / "filetypes.xml"
      if (force || !fileTypesXml.exists) {
        writeFileTypes(fileTypesXml)
      }

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

}

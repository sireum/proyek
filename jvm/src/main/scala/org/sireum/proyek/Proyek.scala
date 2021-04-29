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
import org.sireum.project.{DependencyManager, Module, ModuleProcessor, Project, ProjectUtil, Target}


object Proyek {

  @enum object CompileStatus {
    'Compiled
    'Skipped
    'Error
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

    override def process(shouldProcess: B,
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

  val ignoredPathNames: HashSet[String] = HashSet ++ ISZ[String](
    ".git", ".DS_Store"
  )
  val mainOutDirName: String = "classes"
  val testOutDirName: String = "test-classes"
  val sourcesOutDirName: String = "sources"
  val metaInf: String = "META-INF"
  val manifestMf: String = "MANIFEST.MF"

  def assemble(path: Os.Path,
               outDirName: String,
               project: Project,
               projectName: String,
               jarName: String,
               dm: DependencyManager,
               mainClassNameOpt: Option[String]): Z = {

    val trueF = (_: Os.Path) => T

    val proyekDir = getProyekDir(path, outDirName, projectName, F)
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

    (dm.scalaHome / "lib" / "scala-library.jar").unzipTo(contentDir)

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
        ).run(dm)
        val r: ISZ[(Proyek.CompileStatus.Type, String)] =
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

  def ive(path: Os.Path,
          project: Project,
          projectName: String,
          dm: DependencyManager,
          outDirName: String,
          jbrVersion: String,
          ideaDir: Os.Path,
          isUltimate: B,
          isDev: B,
          force: B): Z = {

    val dotIdea = path / ".idea"
    dotIdea.mkdirAll()

    def writeLibraries(): Unit = {
      val ideaLib = dotIdea / "libraries"
      ideaLib.removeAll()
      ideaLib.mkdirAll()

      def writeLibrary(lib: DependencyManager.Lib): Unit = {
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

      {
        val f = ideaLib / "Sireum.xml"
        f.writeOver(
          st"""<component name="libraryTable">
              |  <library name="Sireum">
              |    <CLASSES>
              |      <root url="jar://$$USER_HOME$$/${relUri(Os.home, dm.sireumJar)}!/" />
              |    </CLASSES>
              |    <JAVADOC />
              |    <SOURCES />
              |  </library>
              |</component>
              |""".render
        )
        println(s"Wrote $f")
      }

      {
        val scalaLibrary = relUri(Os.home, dm.scalaHome / "lib" / "scala-library.jar")
        val scalaCompiler = relUri(Os.home, dm.scalaHome / "lib" / "scala-compiler.jar")
        val scalaReflect = relUri(Os.home, dm.scalaHome / "lib" / "scala-reflect.jar")

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
              |      <root url="https://www.scala-lang.org/api/${dm.scalaVersion}/" />
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
      (path / ".idea_modules").removeAll()

      for (p <- dotIdea.list if p.ext === "iml") {
        p.removeAll()
      }

      val dotIdeaModules = dotIdea / "modules"
      dotIdeaModules.removeAll()
      dotIdeaModules.mkdirAll()

      var moduleEntries = ISZ[ST]()

      def writeModule(m: Module): Unit = {
        moduleEntries = moduleEntries :+
          st"""<module fileurl="file://$$PROJECT_DIR$$/.idea/modules/${m.id}.iml" filepath="$$PROJECT_DIR$$${Os.fileSep}.idea${Os.fileSep}modules${Os.fileSep}${m.id}.iml" />"""
        val deps: ISZ[ST] = for (dep <- m.deps) yield
          st"""<orderEntry type="module" module-name="$dep" exported="" />"""
        val sources: ISZ[ST] = for (src <- ProjectUtil.moduleSources(m)) yield
          st"""<sourceFolder url="file://$$MODULE_DIR$$/../../${relUri(path, src)}" isTestSource="false" />"""
        val resources: ISZ[ST] = for (rsc <- ProjectUtil.moduleResources(m)) yield
          st"""<sourceFolder url="file://$$MODULE_DIR$$/../../${relUri(path, rsc)}" type="java-resource" />"""
        val testSources: ISZ[ST] = for (src <- ProjectUtil.moduleTestSources(m)) yield
          st"""<sourceFolder url="file://$$MODULE_DIR$$/../../${relUri(path, src)}" isTestSource="true" />"""
        val testResources: ISZ[ST] = for (rsc <- ProjectUtil.moduleTestResources(m)) yield
          st"""<sourceFolder url="file://$$MODULE_DIR$$/../../${relUri(path, rsc)}" type="java-test-resource" />"""
        val libs: ISZ[ST] = for (lib <- dm.fetchDiffLibs(m)) yield
          st"""<orderEntry type="library" name="${lib.name}" level="project" exported="" />"""
        val st =
          st"""<?xml version="1.0" encoding="UTF-8"?>
              |<module type="JAVA_MODULE" version="4">
              |  <component name="NewModuleRootManager" inherit-compiler-output="true">
              |    <exclude-output />
              |    <content url="file://$$MODULE_DIR$$/../../${relUri(path, ProjectUtil.moduleBasePath(m))}">
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
      }

      {
        val f = dotIdeaModules / s"$projectName.iml"
        f.writeOver(
          st"""<?xml version="1.0" encoding="UTF-8"?>
              |<module type="JAVA_MODULE" version="4">
              |  <component name="NewModuleRootManager" inherit-compiler-output="true">
              |    <exclude-output />
              |    <content url="file://$$MODULE_DIR$$/../.." />
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
        st"""<module fileurl="file://$$PROJECT_DIR$$/.idea/modules/$projectName.iml" filepath="$$PROJECT_DIR$$${Os.fileSep}.idea${Os.fileSep}modules${Os.fileSep}$projectName.iml" />""";
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
    IVE.writeCodeStyles(dotIdea)
    IVE.writeCompiler(dotIdea)
    IVE.writeScalaCompiler(dotIdea, dm.scalacPlugin, dm.sireumJar)
    IVE.writeScalaSettings(dotIdea)
    IVE.writeInspectionProfiles(dotIdea)
    IVE.writeUiDesigner(dotIdea)
    IVE.writeScriptRunner(dotIdea, projectName)
    IVE.writeWorkspace(dotIdea, dm.sireumHome)
    IVE.writeApplicationConfigs(force, ideaDir, isUltimate, dm.javaHome, dm.javaVersion, jbrVersion, if (isDev) "" else "-dev")
    IVE.writeIveInfo(dotIdea, project, dm.versions)
    return 0
  }

  def publish(path: Os.Path,
              outDirName: String,
              project: Project,
              projectName: String,
              dm: DependencyManager,
              isJs: B,
              orgName: ISZ[String],
              m2Repo: Os.Path,
              version: String,
              symlink: B): Z = {

    @strictpure def shouldCopy(p: Os.Path): B = !ignoredPathNames.contains(p.name)

    val m2Base = m2Repo /+ orgName
    m2Base.mkdirAll()

    val proyekDir = getProyekDir(path, outDirName, projectName, isJs)

    val projectOutDir = proyekDir / "modules"

    val target: Target.Type = if (isJs) Target.Js else Target.Jvm
    for (m <- project.modules.values if m.publishInfoOpt.nonEmpty && m.hasTarget(target)) {

      val mOutDir = projectOutDir / m.id

      val org = st"${(orgName, ".")}".render
      val module = s"${m.id}${if (isJs) dm.sjsSuffix else ""}_${dm.scalaMajorVersion}"

      val pom: String = {
        var deps = ISZ[ST]()

        for (mDep <- m.deps) {
          deps = deps :+ PomTemplate.dep(org, s"$mDep${if (isJs) dm.sjsSuffix else ""}_${dm.scalaMajorVersion}", version)
        }

        for (lib <- dm.fetchDiffLibs(m)) {
          deps = deps :+ PomTemplate.dep(lib.org, lib.module, lib.version)
        }

        val pi = m.publishInfoOpt.get

        PomTemplate.pom(
          name = st"$org.$module".render,
          org = org,
          module = module,
          description = pi.description,
          version = version,
          url = pi.url,
          licenses = for (l <- pi.licenses) yield PomTemplate.license(l.name, l.url, l.distribution),
          developers = for (d <- pi.developers) yield PomTemplate.dev(d.id, d.name),
          dependencies = deps
        ).render

      }

      def writeMainJar(): Unit = {
        val mOutMainDir = mOutDir / mainOutDirName

        for (resource <- ProjectUtil.moduleResources(m)) {
          resource.overlayCopy(mOutMainDir, F, symlink, shouldCopy _, F)
        }

        val mainMetaInf = mOutMainDir / metaInf / manifestMf
        mainMetaInf.up.mkdirAll()
        mainMetaInf.writeOver(
          st"""Manifest-Version: 1.0
              |Created-By: Sireum Proyek
              |""".render
        )

        val mainPomXml = mOutMainDir / metaInf / org / module / "pom.xml"
        mainPomXml.writeOver(pom)

        val m2MainJar = m2Base / module / version / s"$module-$version.jar"
        m2MainJar.up.mkdirAll()
        mOutMainDir.zipTo(m2MainJar)
        println(s"Wrote $m2MainJar")
      }

      def writeSourcesJar(): Unit = {
        val mOutSourcesDir = mOutDir / sourcesOutDirName

        for (source <- ProjectUtil.moduleSources(m)) {
          source.overlayCopy(mOutSourcesDir, F, symlink, shouldCopy _, F)
        }

        for (resource <- ProjectUtil.moduleResources(m)) {
          resource.overlayCopy(mOutSourcesDir, F, symlink, shouldCopy _, F)
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
      }

      def writePom(): Unit = {
        val m2Pom = m2Base / module / version / s"$module-$version.pom"
        m2Pom.up.mkdirAll()

        m2Pom.writeOver(pom)
        println(s"Wrote $m2Pom")
      }

      writeMainJar()
      writeSourcesJar()
      writePom()
    }

    return 0
  }

  def run(path: Os.Path,
          outDirName: String,
          project: Project,
          projectName: String,
          dm: DependencyManager,
          javaOptions: ISZ[String],
          dir: Os.Path,
          className: String,
          args: ISZ[String]): Z = {

    val proyekDir = getProyekDir(path, outDirName, projectName, F)
    val projectOutDir = proyekDir / "modules"

    var classpath = ISZ[String]()

    for (m <- project.modules.values) {
      val mDir = projectOutDir / m.id / mainOutDirName
      if (mDir.exists) {
        classpath = classpath :+ mDir.string
      }

      classpath = classpath ++ (for (resource <- ProjectUtil.moduleResources(m)) yield resource.string)
    }

    for (lib <- dm.libMap.values) {
      classpath = classpath :+ lib.main
    }

    classpath = classpath :+ (dm.scalaHome / "lib" / "scala-library.jar").string

    val javaArgs = javaOptions ++
      ISZ[String]("-classpath", st"${(classpath, Os.pathSep)}".render, className) ++
      args
    val argFile = proyekDir / "java-run-args"
    argFile.writeOver(
      st"${(javaArgs, "\n")}".render)

    val javaExe = dm.javaHome / "bin" / (if (Os.isWin) "java.exe" else "java")
    proc"$javaExe @$argFile".at(dir).console.runCheck()

    return 0
  }

  def test(path: Os.Path,
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


  @strictpure def getProyekDir(path: Os.Path, outDirName: String, projectName: String, isJs: B): Os.Path =
    path / outDirName / s"$projectName${if (isJs) "-js" else ""}"


  @pure def normalizePath(path: String): String = {
    if (Os.isWin) {
      return path
    } else {
      return ops.StringOps(path).replaceAllChars('\\', '/')
    }
  }

  @strictpure def relUri(from: Os.Path, to: Os.Path): String = normalizePath(from.relativize(to).string)

  object IVE {

    val scalaSettings: ST =
      st"""<component name="ScalaProjectSettings">
          |  <option name="autoRunDelay" value="3000" />
          |  <option name="dontCacheCompoundTypes" value="true" />
          |  <option name="inProcessMode" value="false" />
          |  <option name="intInjectionMapping">
          |    <map>
          |      <entry key="xml" value="XML" />
          |    </map>
          |  </option>
          |  <option name="metaTrimMethodBodies" value="false" />
          |  <option name="scFileMode" value="Ammonite" />
          |  <option name="scalaMetaMode" value="Disabled" />
          |  <option name="showNotFoundImplicitArguments" value="false" />
          |  <option name="trailingCommasMode" value="Enabled" />
          |  <option name="treatDocCommentAsBlockComment" value="true" />
          |  <option name="treatScratchFilesAsWorksheet" value="false" />
          |</component>"""

    def writeApplicationConfigs(force: B,
                                ideaDir: Os.Path,
                                isUltimate: B,
                                javaHome: Os.Path,
                                javaVersion: String,
                                jbrVersion: String,
                                devSuffix: String): Unit = {
      val ult: String = if (isUltimate) "-ult" else ""
      val configOptions: Os.Path =
        if (Os.isMac) Os.home / "Library" / "Application Support" / "JetBrains" / s"SireumIVE$ult$devSuffix" / "options"
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
              |        <option name="mySandboxHome" value="$$USER_HOME$$${Os.fileSep}.SireumIVE$devSuffix-sandbox" />
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
              |  <component name="FileTypeManager" version="18">
              |    <extensionMap>
              |      <mapping ext="cmd" type="Scala Worksheet" />
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

      def writeScala(): Unit = {
        val fileTypesXml = configOptions / "scala.xml"
        if (!force && fileTypesXml.exists) {
          return
        }
        fileTypesXml.writeOver(
          st"""<application>
              |  <component name="ScalaSettings">
              |    <option name="SHOW_TYPE_TOOLTIP_ON_MOUSE_HOVER" value="true" />
              |    <option name="COMPILE_SERVER_SDK" value="Project Default" />
              |    <option name="COMPILE_SERVER_MAXIMUM_HEAP_SIZE" value="2048" />
              |    <option name="COMPILE_SERVER_JVM_PARAMETERS" value="-server -Xss2m -XX:MaxInlineLevel=20" />
              |    <option name="COMPILE_SERVER_PARALLELISM" value="4" />
              |  </component>
              |</application>""".render
        )
        println(s"Wrote $fileTypesXml")
      }

      writeJdkTable()
      writeFileTypes()
      writeColors()
      writeScala()

      (Os.home / s".SireumIVE$devSuffix-sandbox").mkdirAll()
    }

    def writeCodeStyles(dotIdea: Os.Path): Unit = {
      val codeStyles = dotIdea / "codeStyles"
      codeStyles.mkdirAll()

      {
        val f = codeStyles / "codeStyleConfig.xml"
        f.writeOver(
          st"""<component name="ProjectCodeStyleConfiguration">
              |  <state>
              |    <option name="USE_PER_PROJECT_SETTINGS" value="true" />
              |  </state>
              |</component>
              |""".render
        )
        println(s"Wrote $f")
      }

      {
        val f = codeStyles / "Project.xml"
        f.writeOver(
          st"""<component name="ProjectCodeStyleConfiguration">
              |  <code_scheme name="Project" version="173">
              |    <option name="FORMATTER_TAGS_ENABLED" value="true" />
              |    <ScalaCodeStyleSettings>
              |      <option name="MULTILINE_STRING_CLOSING_QUOTES_ON_NEW_LINE" value="true" />
              |    </ScalaCodeStyleSettings>
              |  </code_scheme>
              |</component>
              |
              |""".render
        )
        println(s"Wrote $f")
      }
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

    def writeIveInfo(dotIdea: Os.Path, project: Project, versions: HashSMap[String, String]): Unit = {
      {
        val f = dotIdea / "project.json"
        ProjectUtil.store(f, project)
        println(s"Wrote $f")
      }

      {
        val f = dotIdea / "versions.json"
        storeVersions(f, versions)
        println(s"Wrote $f")
      }
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

    def writeScalaCompiler(dotIdea: Os.Path, scalacPlugin: Os.Path, sireumJar: Os.Path): Unit = {
      val f = dotIdea / "scala_compiler.xml"
      val scalacPluginPath = s"$$USER_HOME$$${Os.fileSep}${Os.home.relativize(scalacPlugin)}"
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
            |      <plugin path="$scalacPluginPath" />
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
            |  $scalaSettings
            |</project>"""
          .render)
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

    def writeWorkspace(dotIdea: Os.Path, sireumHome: Os.Path): Unit = {
      val f = dotIdea / "workspace.xml"
      val slangRun = sireumHome / "bin" / (if (Os.isWin) "slang-run.bat" else "slang-run.sh")
      val slangRunLoc = s"$$USER_HOME$$${Os.fileSep}${Os.home.relativize(slangRun)}"
      f.writeOver(
        st"""<?xml version="1.0" encoding="UTF-8"?>
            |<project version="4">
            |  <component name="RunManager">
            |    <configuration default="true" type="ScalaAmmoniteRunConfigurationType" factoryName="Ammonite" singleton="false">
            |      <setting name="execName" value="$slangRunLoc" />
            |      <setting name="fileName" value="" />
            |      <setting name="scriptParameters" value="" />
            |      <method v="2" />
            |    </configuration>
            |    <list>
            |      <item itemvalue="Application.Slang Script Runner" />
            |    </list>
            |  </component>
            |  $scalaSettings
            |</project>""".render
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
          |  <url>https://github.com/$id</url>
          |</developer>"""

    @strictpure def dep(org: String, module: String, version: String): ST =
      st"""<dependency>
          |  <groupId>$org</groupId>
          |  <artifactId>$module</artifactId>
          |  <version>$version</version>
          |</dependency>"""

    @pure def pom(name: String,
                  org: String,
                  module: String,
                  description: String,
                  version: String,
                  url: String,
                  licenses: ISZ[ST],
                  developers: ISZ[ST],
                  dependencies: ISZ[ST]): ST = {
      val urlOps = ops.StringOps(url)
      val i = urlOps.indexOf('/')
      val sshUrl = s"${urlOps.substring(0, i)}:${urlOps.substring(i + 1, url.size)}"
      val r =
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
            |    <connection>scm:git:git://$url.git</connection>
            |    <developerConnection>scm:git:ssh://git@$sshUrl.git</developerConnection>
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
      return r
    }
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
}

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

  @datatype class Lib(val name: String,
                      val main: String,
                      val sourcesOpt: Option[String],
                      val javadocOpt: Option[String])

  val jarSuffix: String = ".jar"
  val sourceJarSuffix: String = "-sources.jar"
  val docJarSuffix: String = "-javadoc.jar"
  val sjsSuffix: String = "_sjs1"
  val ignoredLibraryNames: HashSet[String] = HashSet ++ ISZ[String](
    "org.scala-lang.scala-library", "org.scala-lang.scala-reflect", "org.scala-lang.scala-compiler"
  )

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

  @pure def getVersion(versions: Map[String, String], ivyDep: String): String = {
    versions.get(ops.StringOps(ivyDep).replaceAllChars(':', '%'))match {
      case Some(v) => return v
      case _ => halt(s"Could not find version information for '$ivyDep' in $versions")
    }
  }

  @pure def collectIvyDeps(versions: Map[String, String], project: Project): HashSMap[String, String] = {
    var ivyDeps = HashSMap.empty[String, String]
    for (m <- project.modules.values) {
      for (ivyDep <- m.ivyDeps) {
        val v = getVersion(versions, ivyDep)
        ivyDeps = ivyDeps + ivyDep ~> s"$ivyDep$v"
        val ivyDepOps = ops.StringOps(ivyDep)
        if (ivyDepOps.endsWith("::")) {
          val dep = s"${ivyDepOps.substring(0, ivyDep.size - 2)}$sjsSuffix:"
          ivyDeps = ivyDeps + dep ~> s"$dep$v"
        }
      }
    }
    return ivyDeps
  }

  @pure def normalizePath(path: String): String = {
    if (Os.isWin) {
      return path
    } else {
      return ops.StringOps(path).replaceAllChars('\\', '/')
    }
  }

  @strictpure def relUri(from: Os.Path, to: Os.Path): String = normalizePath(from.relativize(to).string)

  def buildLibMap(ivyDeps: ISZ[String], withSource: B, withDoc: B): HashSMap[String, Lib] = {
    var libMap = HashSMap.empty[String, Lib]
    for (cif <- Coursier.fetchClassifiers(ivyDeps, buildClassifiers(withSource, withDoc))) {
      val name = libName(cif)
      val p = cif.path
      val pNameOps = ops.StringOps(p.string)
      if (!ignoredLibraryNames.contains(name)) {
        var lib: Lib = libMap.get(name) match {
          case Some(l) => l
          case _ => Lib(name, "", None(), None())
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
        libMap = libMap + name ~> lib
      }
    }

    return libMap
  }

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
      println(s"Written $xml")
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
      println(s"Written $xml")
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
    println(s"Written $f")
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
    println(s"Written $f")
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
    println(s"Written $f")
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
    println(s"Written $f")
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
    println(s"Written $f")
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
    println(s"Written $f")
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
    println(s"Written $f")
  }

  def ive(path: Os.Path,
          project: Project,
          versions: Map[String, String],
          projectName: String,
          withSource: B,
          withDoc: B,
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

    val ivyDeps = collectIvyDeps(versions, project)
    val libMap = buildLibMap(ivyDeps.values, withSource, withDoc)

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
        println(s"Written $f")
      }

      for (lib <- libMap.values) {
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
        println(s"Written $f")
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
        println(s"Written $f")
      }
    }

    var tidsCache = HashSMap.empty[String, HashSSet[String]]
    def computeTransitiveIvyDeps(m: Module): ISZ[String] = {
      tidsCache.get(m.id) match {
        case Some(r) => return r.elements
        case _ =>
      }
      var r = HashSSet.empty[String]
      for (mid <- m.deps) {
        r = r ++ computeTransitiveIvyDeps(project.modules.get(mid).get)
      }
      for (id <- m.ivyDeps) {
        r = r + ivyDeps.get(id).get
        val idOps = ops.StringOps(id)
        if (idOps.endsWith("::")) {
          val dep = s"${idOps.substring(0, id.size - 2)}$sjsSuffix:"
          r = r + ivyDeps.get(dep).get
        }
      }
      tidsCache = tidsCache + m.id ~> r
      return r.elements
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
        val libs: ISZ[ST] = for (cif <- Coursier.fetch(computeTransitiveIvyDeps(m)) if !ignoredLibraryNames.contains(libName(cif))) yield
          st"""<orderEntry type="library" name="${libName(cif)}" level="project" />"""
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
        println(s"Written $f")
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
        println(s"Written $f")
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
        println(s"Written $f")
      }
    }

    writeLibraries()
    writeModules()
    writeMisc(dotIdea, outDirName)
    writeCompiler(dotIdea)
    writeScalaCompiler(dotIdea, scalacPlugin)
    writeScalaSettings(dotIdea)
    writeInspectionProfiles(dotIdea)
    writeUiDesigner(dotIdea)
    writeScriptRunner(dotIdea, projectName)
    writeApplicationConfigs(force, ideaDir, javaHome, javaVersion, jbrVersion, if (isDev) "" else "-dev")

    return 0
  }
}

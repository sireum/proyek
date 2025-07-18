// #Sireum
package org.sireum.proyek
/*
 Copyright (c) 2017-2025, Robby, Kansas State University
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

import org.sireum._
import org.sireum.project._
import org.sireum.proyek.Proyek._

object Ive {

  def run(path: Os.Path,
          project: Project,
          projectName: String,
          dm: DependencyManager,
          outDirName: String,
          ideaDir: Os.Path,
          isDev: B,
          force: B,
          isCommunity: B,
          javacOptions: ISZ[String],
          scalacOptions: ISZ[String],
          configPath: Os.Path,
          sandboxPath: Os.Path): Z = {

    val dotIdea = path / ".idea"
    dotIdea.mkdirAll()

    val testLibs: ISZ[ST] = {
      var r = ISZ[ST]()
      for (cif <- dm.fetch(ISZ(s"${DependencyManager.scalaTestKey}${dm.scalaTestVersion}"))) {
        val name = DependencyManager.libName(cif)
        dm.libMap.get(name) match {
          case Some(lib) => r = r :+ st"""<orderEntry type="library" exported="" scope="TEST" name="${lib.name}" level="project" />"""
          case _ =>
        }
      }
      r
    }

    def writeLibraries(): Unit = {
      val ideaLib = dotIdea / "libraries"
      ideaLib.removeAll()
      ideaLib.mkdirAll()

      def writeLibrary(lib: DependencyManager.Lib): Unit = {
        val f = ideaLib / s"${lib.name}.xml"
        val javadocOpt: Option[ST] = lib.javadocOpt match {
          case Some(p) => Some(
            st"""<JAVADOC>
                |  <root url="jar://${Os.path(p).canon}!/" />
                |</JAVADOC>"""
          )
          case _ => None()
        }
        val sourcesOpt: Option[ST] = lib.sourcesOpt match {
          case Some(p) => Some(
            st"""<SOURCES>
                |  <root url="jar://${Os.path(p).canon}!/" />
                |</SOURCES>"""
          )
          case _ => None()
        }
        val st =
          st"""<component name="libraryTable">
              |  <library name="${lib.name}">
              |    <CLASSES>
              |      <root url="jar://${Os.path(lib.main).canon}!/" />
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
              |      <root url="jar://${dm.sireumJar}!/" />
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
        val scalaLibrary = (dm.scalaHome / "lib" / "scala-library.jar").canon
        val scalaCompiler = (dm.scalaHome / "lib" / "scala-compiler.jar").canon
        val scalaReflect = (dm.scalaHome / "lib" / "scala-reflect.jar").canon

        val f = ideaLib / "Scala.xml"
        f.writeOver(
          st"""<component name="libraryTable">
              |  <library name="Scala" type="Scala">
              |    <properties>
              |      <language-level>Scala_2_13</language-level>
              |      <compiler-classpath>
              |        <root url="file://$scalaLibrary" />
              |        <root url="file://$scalaCompiler" />
              |        <root url="file://$scalaReflect" />
              |      </compiler-classpath>
              |    </properties>
              |    <CLASSES>
              |      <root url="jar://$scalaLibrary!/" />
              |      <root url="jar://$scalaReflect!/" />
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

      for (p <- dotIdea.list if p.ext == "iml") {
        p.removeAll()
      }

      val dotIdeaModules = dotIdea / "modules"
      dotIdeaModules.removeAll()
      dotIdeaModules.mkdirAll()

      var moduleEntries = ISZ[ST]()

      def writeModule(m: Module): Unit = {
        moduleEntries = moduleEntries :+
          st"""<module fileurl="file://$$PROJECT_DIR$$/.idea/modules/${m.id}.iml" filepath="$$PROJECT_DIR$$${Os.fileSep}.idea${Os.fileSep}modules${Os.fileSep}${m.id}.iml" />"""
        val deps: ISZ[ST] = for (dep <- project.poset.ancestorsOf(m.id).elements) yield
          st"""<orderEntry type="module" module-name="$dep" />"""
        val sources: ISZ[ST] = for (src <- ProjectUtil.moduleSources(m)) yield
          st"""<sourceFolder url="file://$$MODULE_DIR$$/../../${relUri(path, src)}" isTestSource="false" />"""
        val resources: ISZ[ST] = for (rsc <- ProjectUtil.moduleResources(m)) yield
          st"""<sourceFolder url="file://$$MODULE_DIR$$/../../${relUri(path, rsc)}" type="java-resource" />"""
        val testSources: ISZ[ST] = for (src <- ProjectUtil.moduleTestSources(m)) yield
          st"""<sourceFolder url="file://$$MODULE_DIR$$/../../${relUri(path, src)}" isTestSource="true" />"""
        val testResources: ISZ[ST] = for (rsc <- ProjectUtil.moduleTestResources(m)) yield
          st"""<sourceFolder url="file://$$MODULE_DIR$$/../../${relUri(path, rsc)}" type="java-test-resource" />"""
        var libs: ISZ[ST] = for (lib <- dm.fetchTransitiveLibs(m)) yield
          st"""<orderEntry type="library" name="${lib.name}" level="project" />"""

        var ancestorHasTest = F
        for (superm <- dm.computeTransitiveDeps(m) if ProjectUtil.moduleTestSources(project.modules.get(superm).get).nonEmpty) {
          ancestorHasTest = T
        }
        if (!ancestorHasTest && testSources.nonEmpty) {
          libs = libs ++ testLibs
        }

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

      var seen = HashSet.empty[String]
      var moduleIds = project.poset.rootNodes
      while (moduleIds.nonEmpty) {
        var newModuleIds = HashSSet.empty[String]
        for (mid <- moduleIds if !seen.contains(mid)) {
          seen = seen + mid
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
    IVE.writeMisc(dotIdea, outDirName, javacOptions)
    IVE.writeCodeStyles(dotIdea)
    IVE.writeCompiler(dotIdea, javacOptions)
    IVE.writeScalaCompiler(dotIdea, dm.scalacPlugin, scalacOptions)
    IVE.writeScalaSettings(dotIdea)
    IVE.writeUIDesigner(dotIdea)
    IVE.writeInspectionProfiles(dotIdea)
    IVE.writeScriptRunner(dotIdea, dm.javaHome, projectName)
    IVE.writeWorkspace(dotIdea, dm.sireumHome)
    IVE.writeSbt(dotIdea)
    IVE.writeApplicationConfigs(force, isCommunity, dm.sireumHome, ideaDir, dm.javaHome, dm.javaVersion, isDev,
      configPath, sandboxPath)
    IVE.writeIveInfo(dotIdea, project, dm.versions)
    return 0
  }

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
                                isCommunity: B,
                                sireumHome: Os.Path,
                                ideaDir: Os.Path,
                                javaHome: Os.Path,
                                javaVersion: String,
                                isDev: B,
                                configPath: Os.Path,
                                sandboxPath: Os.Path): Unit = {
      val devSuffix: String = if (isDev) "-dev" else ""
      val configOptions = configPath / "options"
      val configColors = (configOptions.up / "colors").canon
      configOptions.mkdirAll()
      configColors.mkdirAll()

      def writeJdkTable(): Unit = {

        val jdkTableXml = configOptions / "jdk.table.xml"
        if (!force && jdkTableXml.exists) {
          return
        }

        val jbrHome: Os.Path = if (Os.isMac) ideaDir / "jbr" / "Contents" / "Home" else ideaDir / "jbr"

        val jdkModules: Set[String] = Set ++ (for (p <- (javaHome / "jmods").list if p.ext == "jmod") yield
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

        val ideaScala =
          st"""    <jdk version="2">
              |      <name value="Sireum$devSuffix (with Scala Plugin)" />
              |      <type value="IDEA JDK" />
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
              |            <root url="jar://$$APPLICATION_HOME_DIR$$/plugins/terminal/lib/terminal.jar!/" type="simple" />
              |          </root>
              |        </classPath>
              |        <javadocPath>
              |          <root type="composite">
              |            <root url="https://docs.oracle.com/en/java/javase/17/docs/api/" type="simple" />
              |          </root>
              |        </javadocPath>
              |      </roots>
              |      <additional sdk="Jbr">
              |        <option name="mySandboxHome" value="$sandboxPath" />
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
              |$ideaScala
              |  </component>
              |</application>"""

        if (jdkTableXml.up.isWritable) {
          jdkTableXml.writeOver(table.render)
          println(s"Wrote $jdkTableXml")
        }
      }

      def writeFileTypes(): Unit = {
        val fileTypesXml = configOptions / "filetypes.xml"
        if (!force && fileTypesXml.exists) {
          return
        }
        if (fileTypesXml.up.isWritable) {
          fileTypesXml.writeOver(
            st"""<application>
                |  <component name="FileTypeManager" version="18">
                |    <ignoreFiles list=".settings;.opam;acl2;ccl;clion;fmide.app;fmide;graal;idea;idea-ultimate;cargo;rustup;rustrover;Isabelle.app;isabelle;*.pyc;*.pyo;*.rbc;*.yarb;*~;.DS_Store;.git;.hg;.svn;CVS;__pycache__;_svn;vssver.scc;vssver2.scc;vscodium;Brave Browser.app;brave;result-java" />
                |    <extensionMap>
                |      <mapping ext="cmd" type="Scala Worksheet" />
                |      <removed_mapping ext="cmd" approved="true" type="PLAIN_TEXT" />
                |    </extensionMap>
                |  </component>
                |</application>""".render
          )
          println(s"Wrote $fileTypesXml")
        }
      }

      def writeColors(): Unit = {
        for (name <- ISZ[String]("Darcula", "Default")) {
          val f = configColors / s"_@user_$name.icls"
          if (!force && f.exists) {
            return
          }
          if (f.up.isWritable) {
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
      }

      def writeScala(): Unit = {
        val fileTypesXml = configOptions / "scala.xml"
        if (!force && fileTypesXml.exists) {
          return
        }
        if (fileTypesXml.up.isWritable) {
          fileTypesXml.writeOver(
            st"""<application>
                |  <component name="ScalaSettings">
                |    <option name="SHOW_TYPE_TOOLTIP_ON_MOUSE_HOVER" value="true" />
                |    <option name="COMPILE_SERVER_SDK" value="Project Default" />
                |    <option name="COMPILE_SERVER_MAXIMUM_HEAP_SIZE" value="2048" />
                |    <option name="COMPILE_SERVER_JVM_PARAMETERS" value="-server -Xss2m -XX:MaxInlineLevel=20 --enable-native-access=ALL-UNNAMED --sun-misc-unsafe-memory-access=allow" />
                |    <option name="COMPILE_SERVER_PARALLELISM" value="4" />
                |  </component>
                |</application>""".render
          )
          println(s"Wrote $fileTypesXml")
        }
      }

      def writeEditorFont(): Unit = {
        val editorFontXml = configOptions / "editor-font.xml"
        if (!editorFontXml.exists && editorFontXml.up.isWritable) {
          editorFontXml.writeOver(
            st"""<application>
                |  <component name="DefaultFont">
                |    <option name="VERSION" value="1" />
                |    <option name="FONT_SIZE" value="18" />
                |    <option name="FONT_SIZE_2D" value="18.0" />
                |    <option name="FONT_FAMILY" value="Sireum Mono" />
                |    <option name="USE_LIGATURES" value="true" />
                |  </component>
                |</application>""".render
          )
          println(s"Wrote $editorFontXml")
        }
      }

      def writeDisabledPlugins(): Unit = {
        if (!isCommunity) {
          return
        }
        var disableds = HashSSet.empty[String] ++ ISZ(
          "com.intellij.completion.ml.ranking",
          "com.intellij.marketplace.ml",
          "com.intellij.searcheverywhere.ml",
          "com.intellij.turboComplete",
          "org.jetbrains.completion.full.line",
          "PerforceDirectPlugin",
          "com.android.tools.gradle.dcl",
          "com.intellij.compose",
          "com.intellij.marketplace",
          "com.intellij.notebooks.core",
          "com.intellij.tasks",
          "com.jetbrains.codeWithMe",
          "hg4idea",
          "intellij.git.commit.modal",
          "intellij.jupyter",
          "org.intellij.qodana",
          "org.jetbrains.idea.eclipse",
          "org.jetbrains.plugins.kotlin.jupyter",
          "training"
        )
        val disabledPlugins = configPath / "disabled_plugins.txt"
        if (disabledPlugins.exists) {
          for (line <- disabledPlugins.readLineStream) {
            disableds = disableds + ops.StringOps(line).trim
          }
        }
        disabledPlugins.writeOver(st"""${(disableds.elements, "\n")}""".render)
        println(s"Wrote $disabledPlugins")
      }

      writeJdkTable()
      writeFileTypes()
      writeColors()
      writeScala()
      writeEditorFont()
      writeDisabledPlugins()

      sandboxPath.mkdirAll()
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

    def writeCompiler(dotIdea: Os.Path, javacOptions: ISZ[String]): Unit = {
      val f = dotIdea / "compiler.xml"
      f.writeOver(
        st"""<?xml version="1.0" encoding="UTF-8"?>
            |<project version="4">
            |  <component name="CompilerConfiguration">
            |    <option name="USE_RELEASE_OPTION" value="false" />
            |  </component>
            |  <component name="JavacSettings">
            |    <option name="ADDITIONAL_OPTIONS_STRING" value="${(javacOptions, " ")}" />
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
            |    <inspection_tool class="NonAsciiCharacters" enabled="false" level="WARNING" enabled_by_default="false" />
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

    def writeMisc(dotIdea: Os.Path, outDirName: String, javacOptions: ISZ[String]): Unit = {
      var javaVersion: String = "17"
      var i = 0
      while (i < javacOptions.size) {
        if (i + 1 < javacOptions.size) {
          javacOptions(i) match {
            case string"release" => javaVersion = javacOptions(i + 1)
            case string"source" => javaVersion = javacOptions(i + 1)
            case _ =>
          }
        }
        i = i + 1
      }
      val f = dotIdea / "misc.xml"
      f.writeOver(
        st"""<?xml version="1.0" encoding="UTF-8"?>
            |<project version="4">
            |  <component name="ProjectRootManager" version="2" languageLevel="JDK_$javaVersion" project-jdk-name="Java" project-jdk-type="JavaSDK">
            |    <output url="file://$$PROJECT_DIR$$/$outDirName" />
            |  </component>
            |</project>
            |""".render
      )
      println(s"Wrote $f")
    }

    def writeScalaCompiler(dotIdea: Os.Path, scalacPlugin: Os.Path, scalacOptions: ISZ[String]): Unit = {
      val f = dotIdea / "scala_compiler.xml"
      val scalacPluginPath = s"$scalacPlugin"
      f.writeOver(
        st"""<?xml version="1.0" encoding="UTF-8"?>
            |<project version="4">
            |  <component name="ScalaCompilerConfiguration">
            |    <option name="postfixOps" value="true" />
            |    <option name="deprecationWarnings" value="true" />
            |    <option name="uncheckedWarnings" value="true" />
            |    <option name="featureWarnings" value="true" />
            |    <parameters>
            |      ${(for (opt <- scalacOptions) yield st"""<parameter value="$opt" />""", "\n")}
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

    def writeUIDesigner(dotIdea: Os.Path): Unit = {
      val f = dotIdea / "uiDesigner.xml"
      f.writeOver(
        st"""<?xml version="1.0" encoding="UTF-8"?>
            |<project version="4">
            |  <component name="uidesigner-configuration">
            |    <option name="INSTRUMENT_CLASSES" value="false" />
            |    <option name="DEFAULT_FIELD_ACCESSIBILITY" value="public" />
            |  </component>
            |</project>""".render
      )
      println(s"Wrote $f")
    }

    def writeSbt(dotIdea: Os.Path): Unit = {
      val f = dotIdea / "sbt.xml"
      f.writeOver(
        st"""<?xml version="1.0" encoding="UTF-8"?>
            |<project version="4">
            |  <component name="ScalaSbtSettings">
            |    <option name="customLauncherPath" />
            |    <option name="customVMPath" />
            |    <option name="vmParameters" value="--enable-native-access=ALL-UNNAMED" />
            |  </component>
            |</project>""".render
      )
      println(s"Wrote $f")
    }

    def writeScriptRunner(dotIdea: Os.Path, javaHome: Os.Path, name: String): Unit = {
      val runConfigurations = dotIdea / "runConfigurations"
      runConfigurations.mkdirAll()

      val f = runConfigurations / "Slang_Script_Runner.xml"
      f.writeOver(
        st"""<component name="ProjectRunConfigurationManager">
            |  <configuration default="false" name="Slang Script Runner" type="Application" factoryName="Application" singleton="false">
            |    <envs>
            |      <env name="JAVA_HOME" value="$javaHome" />
            |    </envs>
            |    <option name="MAIN_CLASS_NAME" value="org.sireum.Sireum" />
            |    <module name="$name" />
            |    <option name="PROGRAM_PARAMETERS" value="slang run $$FilePath$$" />
            |    <option name="VM_PARAMETERS" value="--enable-native-access=ALL-UNNAMED" />
            |    <method v="2" />
            |  </configuration>
            |</component>
            |""".render
      )
      println(s"Wrote $f")
    }

    def writeWorkspace(dotIdea: Os.Path, sireumHome: Os.Path): Unit = {
      val f = dotIdea / "workspace.xml"
      val slangRun = (sireumHome / "bin" / (if (Os.isWin) "slang-run.bat" else "slang-run.sh")).canon
      f.writeOver(
        st"""<?xml version="1.0" encoding="UTF-8"?>
            |<project version="4">
            |  <component name="RunManager">
            |    <configuration default="true" type="ScalaAmmoniteRunConfigurationType" factoryName="Ammonite" singleton="false">
            |      <setting name="execName" value="$slangRun" />
            |      <setting name="fileName" value="" />
            |      <setting name="scriptParameters" value="" />
            |      <method v="2" />
            |    </configuration>
            |    <configuration default="true" type="Application" factoryName="Application">
            |      <option name="VM_PARAMETERS" value="--enable-native-access=ALL-UNNAMED" />
            |      <method v="2">
            |        <option name="Make" enabled="true" />
            |      </method>
            |    </configuration>
            |    <configuration default="true" type="JUnit" factoryName="JUnit">
            |      <option name="MAIN_CLASS_NAME" value="" />
            |      <option name="METHOD_NAME" value="" />
            |      <option name="TEST_OBJECT" value="class" />
            |      <option name="VM_PARAMETERS" value="-ea --enable-native-access=ALL-UNNAMED" />
            |      <method v="2">
            |        <option name="Make" enabled="true" />
            |      </method>
            |    </configuration>
            |    <configuration default="true" type="JarApplication">
            |      <option name="ALTERNATIVE_JRE_PATH" />
            |      <method v="2" />
            |    </configuration>
            |    <configuration default="true" type="ScalaTestRunConfiguration" factoryName="ScalaTest" show_console_on_std_err="false" show_console_on_std_out="false">
            |      <option name="VMParameters" value="--enable-native-access=ALL-UNNAMED" />
            |      <option name="allowRunningInParallel" value="false" />
            |      <option name="alternativeJrePath" />
            |      <option name="alternativeJrePathEnabled" value="false" />
            |      <option name="classpathModifications">
            |        <list />
            |      </option>
            |      <option name="envFilePaths">
            |        <list />
            |      </option>
            |      <option name="envs">
            |        <map />
            |      </option>
            |      <option name="passParentEnvs" value="true" />
            |      <option name="programParameters" value="" />
            |      <option name="projectPathOnTarget" />
            |      <option name="selectedOptions">
            |        <list />
            |      </option>
            |      <option name="testKind" value="Class" />
            |      <option name="workingDirectory" value="$$PROJECT_DIR$$" />
            |      <option name="javaOptions" value="--enable-native-access=ALL-UNNAMED" />
            |      <option name="shortenClasspath" value="NONE" />
            |      <method v="2">
            |        <option name="Make" enabled="true" />
            |      </method>
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

}
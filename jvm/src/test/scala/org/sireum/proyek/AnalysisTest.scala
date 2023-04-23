/*
 Copyright (c) 2017-2023, Robby, Kansas State University
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
import org.sireum.lang.{ast => AST}
import org.sireum.lang.symbol.Info
import org.sireum.lang.tipe.TypeHierarchy
import org.sireum.project._
import org.sireum.proyek.Analysis.ModuleProcessor
import org.sireum.proyek.ModuleProcessor.ProcessResult
import org.sireum.test._
import org.sireum.logika.LogikaTest
import org.sireum.message.{Message, Position}

object AnalysisTest {

  class ReporterImpl(var _messages: ISZ[Message]) extends logika.Logika.Reporter {
    private var isClonable: scala.Boolean = true
    private var isOwned: scala.Boolean = false
    var _ignore: B = F
    var isIllFormed: B = F

    override def $clonable: Boolean = isClonable

    override def $clonable_=(b: Boolean): this.type = {
      isClonable = false
      this
    }

    override def $owned: scala.Boolean = isOwned

    override def $owned_=(b: scala.Boolean): this.type = {
      isOwned = b
      this
    }

    override def combine(other: logika.Logika.Reporter): logika.Logika.Reporter = {
      other match {
        case other: ReporterImpl =>
          _messages = _messages ++ other._messages
          isIllFormed = isIllFormed || other.isIllFormed
          return this
      }
    }

    override def $clone: ReporterImpl = {
      val r = new ReporterImpl(_messages)
      r.isIllFormed = isIllFormed
      r
    }

    override def string: String = {
      return "ReporterImpl"
    }

    override def illFormed(): Unit = {
      isIllFormed = T
    }

    override def state(plugins: ISZ[logika.plugin.ClaimPlugin], posOpt: Option[Position], context: ISZ[String],
                       th: TypeHierarchy, s: logika.State, atLinesFresh: B): Unit = {}

    override def inform(pos: Position, kind: org.sireum.logika.Logika.Reporter.Info.Kind.Type, message: String): Unit = {}

    override def query(pos: Position, title: String, time: Z, forceReport: B, detailElided: B, r: logika.Smt2Query.Result): Unit = {}

    override def timing(desc: String, timeInMs: Z): Unit = {}

    override def empty: logika.Logika.Reporter = {
      return new ReporterImpl(ISZ())
    }

    override def messages: ISZ[Message] = {
      return _messages
    }

    override def ignore: B = {
      return _ignore
    }

    override def setIgnore(newIgnore: B): Unit = {
      _ignore = newIgnore
    }

    override def setMessages(newMessages: ISZ[Message]): Unit = synchronized {
      _messages = newMessages
    }

    override def report(m: Message): Unit = synchronized {
      super.report(m)
    }
  }

  val versions: String = $internal.RC.text(Vector("../../../../../../../../versions.properties")) { (_, _) => T }.head._2

  class TempProject(val root: Os.Path) {
    val test1: Os.Path = root / "test1"
    val test1SrcDir: Os.Path = test1 / "src" / "test1"
    val test2: Os.Path = root / "test2"
    val test2SrcDir: Os.Path = test2 / "src" / "test2"
    val outDir: Os.Path = root / "out"

    val project: Project = {
      var prj = Project.empty
      outDir.mkdirAll()

      test1SrcDir.mkdirAll()
      prj = prj + Module(
        id = test1.name,
        basePath = test1.string,
        subPathOpt = None() ,
        deps = ISZ(),
        targets = ISZ(Target.Jvm),
        ivyDeps = ISZ(DependencyManager.libraryKey),
        sources = ISZ("src"),
        resources = ISZ(),
        testSources = ISZ(),
        testResources = ISZ(),
        publishInfoOpt = None()
      )

      test2SrcDir.mkdirAll()
      prj = prj + Module(
        id = test2.name,
        basePath = test2.string,
        subPathOpt = None() ,
        deps = ISZ("test1"),
        targets = ISZ(Target.Jvm),
        ivyDeps = ISZ(),
        sources = ISZ("src"),
        resources = ISZ(),
        testSources = ISZ(),
        testResources = ISZ(),
        publishInfoOpt = None()
      )

      prj
    }

    val sireumHome: Os.Path = {
      Os.env("SIREUM_HOME") match {
        case Some(p) => Os.path(p)
        case _ => halt("Please set SIREUM_HOME env var")
      }
    }

    val scalaHome: Os.Path = sireumHome / "bin" / "scala"

    val javaHome: Os.Path = Os.kind match {
      case Os.Kind.Mac => sireumHome / "bin" / "mac" / "java"
      case Os.Kind.Linux => sireumHome / "bin" / "linux" / "java"
      case Os.Kind.Win => sireumHome / "bin" / "win" / "java"
      case Os.Kind.LinuxArm => sireumHome / "bin" / "linux" / "arm" / "java"
      case Os.Kind.Unsupported => halt("Unsupported")
    }

    val dm: DependencyManager = DependencyManager(
      project = project,
      versions = {
        val prop = new java.util.Properties
        prop.load(new java.io.StringReader(versions.value))
        var m = HashSMap.empty[String, String]
        val ks = prop.keys
        while (ks.hasMoreElements) {
          val k = ks.nextElement
          m = m + k.toString.replace('%', ':').string ~> prop.get(k).string
        }
        m = m + DependencyManager.libraryKey ~> "SNAPSHOT"
        m
      },
      isJs = F,
      withSource = F,
      withDoc = F,
      javaHome = javaHome,
      scalaHome = scalaHome,
      sireumHome = sireumHome,
      cacheOpt = None(),
    )

    val test1Lmp: ModuleProcessor = ModuleProcessor(
      root = root,
      module = project.modules.get(test1.name).get,
      force = F,
      par = 1,
      strictAliasing = T,
      followSymLink = F,
      verbose = F,
      outDir = outDir,
    )

    val test2Lmp: ModuleProcessor = ModuleProcessor(
      root = root,
      module = project.modules.get(test2.name).get,
      force = F,
      par = 1,
      strictAliasing = T,
      followSymLink = F,
      verbose = F,
      outDir = outDir,
    )
  }

  def sysid(o: AnyRef): Int = System.identityHashCode(o)
}

import AnalysisTest._

class AnalysisTest extends TestSuite {

  val tests = Tests {

    * - {
      val tempProject = new TempProject(Os.tempDirFix("logika-proyek-test"))
      val test1Slang = tempProject.test1SrcDir / "Test1.scala"
      val test1Content =
        st"""// #Sireum
            |package test1
            |import org.sireum._
            |
            |object Test1 {
            |  val x: Z = 1
            |
            |  def foo(): Unit = {
            |    println(x)
            |  }
            |}""".render
      test1Slang.writeOver(test1Content)
      val test1Sources = ISZ(test1Slang)

      val test2Slang = tempProject.test2SrcDir / "Test2.scala"
      val test2SlangContent =
        st"""// #Sireum
            |package test2
            |import org.sireum._
            |
            |object Test2 {
            |  val y: Z = 1
            |
            |  def bar(): Unit = {
            |    println(test1.Test1.x)
            |    println(y)
            |  }
            |}""".render
      val test2SlangContent2 =
        st"""// #Sireum
            |package test2
            |import org.sireum._
            |
            |object Test2 {
            |  val y: B = T
            |
            |  def bar(): Unit = {
            |    println(test1.Test1.x)
            |    println(y)
            |  }
            |}""".render
      test2Slang.writeOver(test2SlangContent)
      val test2Sources = ISZ(test2Slang)

      val config = LogikaTest.config
      val files = HashSMap.empty + test2Slang.string ~> test2SlangContent
      val vi = proyek.Analysis.Info(
        cacheInput = F,
        uriMap = HashMap.empty,
        thMap = HashMap.empty,
        files = files,
        vfiles = files.keys,
        line = 0,
        all = F,
        verify = T,
        verbose = F,
        sanityCheck = T,
        config = config,
        plugins = org.sireum.logika.Logika.defaultPlugins,
        skipMethods = ISZ(),
        skipTypes = ISZ()
      )
      val cache = logika.NoTransitionSmt2Cache.create

      val reporter = new ReporterImpl(ISZ())
      val ProcessResult(vi2, _, r2, _) = tempProject.test1Lmp.process(vi, cache, T, HashSet.empty, tempProject.dm,
        test1Sources, ISZ(), reporter)
      assert(r2)
      assert(reporter.messages.isEmpty)
      assert(vi2.thMap.get("test1").nonEmpty)
      assert(vi2.thMap.get("test2").isEmpty)

      val ProcessResult(vi3, _, r3, _) = tempProject.test2Lmp.process(vi2, cache, T, HashSet.empty, tempProject.dm,
        test2Sources, ISZ(), reporter)
      assert(r3)
      assert(reporter.messages.isEmpty)
      assert(sysid(vi3.thMap.get("test1").get) == sysid(vi2.thMap.get("test1").get))
      assert(vi3.thMap.get("test2").nonEmpty)

      val ProcessResult(vi4, _, r4, _) = tempProject.test1Lmp.process(
        vi3(files = vi3.files + test2Slang.string ~> test2SlangContent2), cache, F, HashSet.empty, tempProject.dm,
        test1Sources, ISZ(), reporter)
      assert(!r4)
      assert(reporter.messages.isEmpty)
      assert(sysid(vi4.thMap.get("test1").get) == sysid(vi3.thMap.get("test1").get))
      assert(sysid(vi4.thMap.get("test2").get) == sysid(vi3.thMap.get("test2").get))

      val ProcessResult(vi5, _, r5, _) = tempProject.test2Lmp.process(vi4, cache, F, HashSet.empty, tempProject.dm,
        test2Sources, ISZ(), reporter)
      assert(!r5)
      assert(reporter.messages.isEmpty)
      assert(sysid(vi5.thMap.get("test1").get) == sysid(vi4.thMap.get("test1").get))
      assert(sysid(vi5.thMap.get("test2").get) != sysid(vi4.thMap.get("test2").get))

      val yFQ = ISZ[String]("test2", "Test2", "y")
      val vi4th2 = vi4.thMap.get("test2").get
      assert(vi4th2.nameMap.get(yFQ).get.asInstanceOf[Info.Var].typedOpt.get == AST.Typed.z)

      val vi5th2 = vi5.thMap.get("test2").get
      assert(vi5th2.nameMap.get(yFQ).get.asInstanceOf[Info.Var].typedOpt.get == AST.Typed.b)

      tempProject.root.removeAll()
    }
  }
}

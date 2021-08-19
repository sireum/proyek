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
import org.sireum.lang.FrontEnd
import org.sireum.lang.symbol.Resolver
import org.sireum.lang.{ast => AST}
import org.sireum.lang.tipe.{TypeChecker, TypeHierarchy, TypeOutliner}
import org.sireum.logika.plugin.Plugin
import org.sireum.logika.{Config, Logika, Smt2, Smt2Impl}
import org.sireum.message.Message
import org.sireum.project._

object LogikaVerifier {

  @datatype class VerificationInfo(val thMap: HashMap[String, TypeHierarchy],
                                   val files: HashSMap[String, String],
                                   val messages: ISZ[Message],
                                   val lineOpt: Option[Z],
                                   val all: B,
                                   val stop: B,
                                   val verify: B,
                                   val config: Config,
                                   val plugins: ISZ[Plugin],
                                   val skipMethods: ISZ[String],
                                   val skipTypes: ISZ[String])

  @record class LogikaModuleProcessor(val root: Os.Path,
                                      val module: Module,
                                      val par: B,
                                      val strictAliasing: B,
                                      val followSymLink: B,
                                      val outDir: Os.Path) extends ModuleProcessor[VerificationInfo, Smt2.Cache] {

    @strictpure def sha3: B = F

    @strictpure def force: B = F

    @pure override def fileFilter(file: Os.Path): B = {
      val ext = file.ext
      if (ext != "scala" && ext != "slang") {
        return F
      }
      var cis = ISZ[C]()
      for (c <- file.readCStream.takeWhile((c: C) => c != '\n') if !c.isWhitespace) {
        cis = cis :+ c
      }
      val firstLine = ops.StringOps(conversions.String.fromCis(cis))
      return firstLine.contains("#Sireum")
    }

    override def process(info: VerificationInfo,
                         cache: Smt2.Cache,
                         shouldProcess: B,
                         dm: DependencyManager,
                         sourceFiles: ISZ[Os.Path],
                         testSourceFiles: ISZ[Os.Path]): (VerificationInfo, B) = {
      def toInput(p: Os.Path): FrontEnd.Input = {
        val uri = p.toUri
        info.files.get(p.string) match {
          case Some(content) => return FrontEnd.Input(content, Some(uri), 0)
          case _ => return FrontEnd.Input(p.read, Some(uri), p.lastModified)
        }
      }

      val sourceFilePaths: ISZ[String] = for (p <- sourceFiles ++ testSourceFiles) yield p.string
      val checkFilePaths: ISZ[String] =
        if (info.all) sourceFilePaths
        else ops.ISZOps(sourceFilePaths).filter((p: String) => info.files.contains(p))
      val checkFileUris = HashSet ++ (for (p <- checkFilePaths) yield Os.path(p).toUri)
      val (nameMap, typeMap, info2, changed): (Resolver.NameMap, Resolver.TypeMap, VerificationInfo, B) =
        info.thMap.get(module.id) match {
          case Some(th) if !info.all && !shouldProcess =>
            if (checkFilePaths.isEmpty) {
              return (info, F)
            } else {
              var nm = th.nameMap
              var tm = th.typeMap
              for (info <- nm.values if info.posOpt.nonEmpty) {
                info.posOpt.get.uriOpt match {
                  case Some(uri) if checkFileUris.contains(uri) => nm = nm - ((info.name, info))
                  case _ =>
                }
              }
              for (info <- tm.values if info.posOpt.nonEmpty) {
                info.posOpt.get.uriOpt match {
                  case Some(uri) if checkFileUris.contains(uri) => tm = tm - ((info.name, info))
                  case _ =>
                }
              }
              val inputs = ops.ISZOps(for (p <- checkFilePaths) yield toInput(Os.path(p)))
              val q = ops.ISZOps(
                if (par) inputs.parMap(FrontEnd.parseGloballyResolve _)
                else inputs.map(FrontEnd.parseGloballyResolve _)).
                foldLeft(FrontEnd.combineParseResult _, (ISZ[Message](), ISZ[AST.TopUnit.Program](), nm, tm))
              (q._3, q._4, info(messages = info.messages ++ q._1), T)
            }
          case _ =>
            var nm: Resolver.NameMap = HashMap.empty
            var tm: Resolver.TypeMap = HashMap.empty
            for (mid <- dm.project.poset.parentsOf(module.id).elements) {
              val mth = info.thMap.get(mid).get
              nm = nm ++ mth.nameMap.entries
              tm = tm ++ mth.typeMap.entries
            }
            val inputs = ops.ISZOps(for (p <- sourceFiles ++ testSourceFiles) yield toInput(p))
            val q = ops.ISZOps(
              if (par) inputs.parMap(FrontEnd.parseGloballyResolve _)
              else inputs.map(FrontEnd.parseGloballyResolve _)).
              foldLeft(FrontEnd.combineParseResult _, (ISZ[Message](), ISZ[AST.TopUnit.Program](), nm, tm))
            nm = q._3
            tm = q._4
            if (tm.get(AST.Typed.isName).isEmpty) {
              val mth = FrontEnd.checkedLibraryReporter._1.typeHierarchy
              nm = nm ++ mth.nameMap.entries
              tm = tm ++ mth.typeMap.entries
            }
            (nm, tm, info(messages = info.messages ++ q._1), T)
        }
      val rep = Logika.Reporter.create
      rep.reports(info2.messages)
      if (!rep.hasError && changed) {
        var th = TypeHierarchy.build(T, TypeHierarchy(nameMap, typeMap, Poset.empty, HashMap.empty), rep)
        if (!rep.hasError) {
          th = TypeOutliner.checkOutline(par, strictAliasing, th, rep)
        }
        if (!rep.hasError) {
          var nm = HashMap.empty[ISZ[String], lang.symbol.Info]
          var tm = HashMap.empty[ISZ[String], lang.symbol.TypeInfo]
          if (nm.nonEmpty || tm.nonEmpty) {
            for (info <- th.nameMap.values) {
              info.posOpt.get.uriOpt match {
                case Some(uri) if checkFileUris.contains(uri) => nm = nm + info.name ~> info
                case _ =>
              }
            }
            for (info <- th.typeMap.values) {
              info.posOpt.get.uriOpt match {
                case Some(uri) if checkFileUris.contains(uri) => tm = tm + info.name ~> info
                case _ =>
              }
            }
            th = TypeChecker.checkComponents(par, strictAliasing, th, nm, tm, rep)
          }
        }
        val info3 = info2(messages = rep.messages)
        if (rep.hasError) {
          return (info3(stop = T), changed)
        }
        val newFiles = info2.files -- checkFilePaths
        val info4 = info3(
          thMap = info3.thMap + module.id ~> th,
          files = newFiles,
          stop = info3.files.nonEmpty ->: newFiles.isEmpty
        )
        if (!info.verify) {
          return (info4, changed)
        }
        val config = info.config
        Logika.checkTypedPrograms(
          verifyingStartTime = 0,
          fileSet = checkFileUris,
          config = config,
          th = th,
          smt2f = (th: TypeHierarchy) =>
            Smt2Impl.create(config.smt2Configs, th, config.timeoutInMs, config.charBitWidth,
              config.intBitWidth, config.simplifiedQuery, rep),
          cache = cache,
          reporter = rep,
          par = par,
          plugins = info.plugins,
          line = info.lineOpt.getOrElseEager(0),
          skipMethods = info.skipMethods,
          skipTypes = info.skipTypes
        )
        return (info4(messages = rep.messages), T)
      } else {
        return (info2, changed)
      }
    }
  }

}
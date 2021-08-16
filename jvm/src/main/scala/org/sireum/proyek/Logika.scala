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
import org.sireum.message.{Message, Reporter}
import org.sireum.project._

object Logika {

  @datatype class VerificationInfo(val map: HashMap[String, TypeHierarchy],
                                   val files: HashMap[String, String],
                                   val messages: ISZ[Message],
                                   val lineOpt: Option[Z],
                                   val stop: B)

  @datatype class LogikaModuleProcessor(val root: Os.Path,
                                        val module: Module,
                                        val par: B,
                                        val followSymLink: B,
                                        val outDir: Os.Path) extends ModuleProcessor[VerificationInfo] {

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
                         shouldProcess: B,
                         dm: DependencyManager,
                         sourceFiles: ISZ[Os.Path],
                         testSourceFiles: ISZ[Os.Path]): (VerificationInfo, B) = {
      val fileUriSet = HashSSet.empty[String] ++ (for (p <- sourceFiles ++ testSourceFiles) yield p.toUri)
      val checkUriSet = HashSSet.empty[String] ++ (for (p <- info.files.keys) yield Os.path(p).toUri)
      val thOpt = info.map.get(module.id)
      val (nameMap, typeMap, programs, info2, changed): (Resolver.NameMap, Resolver.TypeMap, ISZ[AST.TopUnit.Program], VerificationInfo, B) = thOpt match {
        case Some(th) if !shouldProcess =>
          var nm = th.nameMap
          var tm = th.typeMap
          val checkFiles = info.files -- fileUriSet.elements
          if (checkFiles.nonEmpty) {
            val inputs = ops.ISZOps(for (pair <- checkFiles.entries) yield FrontEnd.Input(pair._2, Some(Os.path(pair._1).toUri), 0))
            for (info <- nm.values) {
              info.posOpt.get.uriOpt match {
                case Some(uri) if fileUriSet.contains(uri) && checkUriSet.contains(uri) => nm = nm - ((info.name, info))
                case _ =>
              }
            }
            for (info <- tm.values) {
              info.posOpt.get.uriOpt match {
                case Some(uri) if fileUriSet.contains(uri) && checkUriSet.contains(uri) => tm = tm - ((info.name, info))
                case _ =>
              }
            }
            val results = ops.ISZOps(
              if (par) inputs.parMap(FrontEnd.parseGloballyResolve _)
              else inputs.map(FrontEnd.parseGloballyResolve _))
            val q = results.
              foldLeft(FrontEnd.combineParseResult _, (ISZ[Message](), ISZ[AST.TopUnit.Program](), nm, tm))
            (q._3, q._4, q._2, info(messages = info.messages ++ q._1), T)
          } else {
            (nm, tm, ISZ(), info, F)
          }
        case _ =>
          def toInput(p: Os.Path): FrontEnd.Input = {
            val uri = p.toUri
            info.files.get(p.string) match {
              case Some(content) => return FrontEnd.Input(content, Some(uri), p.lastModified)
              case _ => return FrontEnd.Input(p.read, Some(uri), p.lastModified)
            }
          }
          val inputs = ops.ISZOps(for (p <- sourceFiles ++ testSourceFiles) yield toInput(p))
          val results = ops.ISZOps(
            if (par) inputs.parMap(FrontEnd.parseGloballyResolve _)
            else inputs.map(FrontEnd.parseGloballyResolve _))
          val (nm, tm) = Resolver.addBuiltIns(HashMap.empty, HashMap.empty)
          val q = results.
            foldLeft(FrontEnd.combineParseResult _, (ISZ[Message](), ISZ[AST.TopUnit.Program](), nm, tm))
          (q._3, q._4,
            for (program <- q._2 if checkUriSet.contains(program.fileUriOpt.get)) yield program,
            info(messages = info.messages ++ q._1), T)
      }
      val rep = Reporter.create
      if (changed) {
        var th = TypeHierarchy.build(T, TypeHierarchy(nameMap, typeMap, Poset.empty, HashMap.empty), rep)
        if (!rep.hasError) {
          th = TypeOutliner.checkOutline(T, T, th, rep)
        }
        if (!rep.hasError) {
          var nm = HashMap.empty[ISZ[String], lang.symbol.Info]
          var tm = HashMap.empty[ISZ[String], lang.symbol.TypeInfo]
          if (nm.nonEmpty || tm.nonEmpty) {
            for (info <- th.nameMap.values) {
              info.posOpt.get.uriOpt match {
                case Some(uri) if fileUriSet.contains(uri) && checkUriSet.contains(uri) => nm = nm + info.name ~> info
                case _ =>
              }
            }
            for (info <- th.typeMap.values) {
              info.posOpt.get.uriOpt match {
                case Some(uri) if fileUriSet.contains(uri) && checkUriSet.contains(uri) => tm = tm + info.name ~> info
                case _ =>
              }
            }
            th = TypeChecker.checkComponents(T, T, th, nm, tm, rep)
          }
        }
        val info3 = info2(messages = info2.messages ++ rep.messages)
        if (ops.ISZOps(info3.messages).exists((m: Message) => m.isError || m.isInternalError)) {
          return (info3(stop = T), changed)
        }
        val newFiles = info2.files -- (checkUriSet -- fileUriSet.elements).elements
        val info4 = info3(
          map = info3.map + module.id ~> th,
          files = newFiles,
          stop = info3.files.nonEmpty ->: newFiles.isEmpty
        )
        // TODO: Verify programs
        return (info4, changed)
      } else {
        return (info2, changed)
      }
    }
  }

}

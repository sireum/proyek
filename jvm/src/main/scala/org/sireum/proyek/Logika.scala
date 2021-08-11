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
import org.sireum.lang.{ast => AST}
import org.sireum.lang.tipe.TypeHierarchy
import org.sireum.message.{Message, Reporter}
import org.sireum.project._

object Logika {

  @datatype class VerificationInfo(val map: HashMap[String, TypeHierarchy],
                                   val files: HashMap[String, String],
                                   val messages: ISZ[Message],
                                   val stop: B)

  @datatype class LogikaModuleProcessor(val root: Os.Path,
                                        val module: Module,
                                        val par: B,
                                        val followSymLink: B,
                                        val outDir: Os.Path,
                                       ) extends ModuleProcessor[VerificationInfo] {

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

    override def process(o: VerificationInfo,
                         shouldProcess: B,
                         dm: DependencyManager,
                         sourceFiles: ISZ[Os.Path],
                         testSourceFiles: ISZ[Os.Path]): (VerificationInfo, B) = {
      var info = o
      val fileUriSet = HashSSet.empty[String] ++ (for (p <- sourceFiles ++ testSourceFiles) yield p.toUri)
      val checkUriSet = HashSSet.empty[String] ++ (for (p <- info.files.keys) yield Os.path(p).toUri)
      val shouldCheck = ops.ISZOps(info.files.keys).exists((p: String) => fileUriSet.contains(p))
      var nameMap = HashMap.empty[ISZ[String], org.sireum.lang.symbol.Info]
      var typeMap = HashMap.empty[ISZ[String], org.sireum.lang.symbol.TypeInfo]
      var programs = ISZ[AST.TopUnit.Program]()
      val thOpt = info.map.get(module.id)
      thOpt match {
        case Some(th) if !shouldProcess =>
          nameMap = th.nameMap
          typeMap = th.typeMap
          if (shouldCheck) {
            for (info <- nameMap.values) {
              info.posOpt.get.uriOpt match {
                case Some(uri) if fileUriSet.contains(uri) && checkUriSet.contains(uri)  =>
                  nameMap = nameMap - ((info.name, info))
                case _ =>
              }
            }
            for (info <- typeMap.values) {
              info.posOpt.get.uriOpt match {
                case Some(uri) if fileUriSet.contains(uri) && checkUriSet.contains(uri) =>
                  typeMap = typeMap - ((info.name, info))
                case _ =>
              }
            }
          }
          val inputs = ops.ISZOps(for (pair <- info.files.entries) yield FrontEnd.Input(pair._2, Some(Os.path(pair._1).toUri), 0))
          val results = ops.ISZOps(
            if (par) inputs.parMap(FrontEnd.parseGloballyResolve _)
            else inputs.map(FrontEnd.parseGloballyResolve _))
          val (messages, ps, nm, tm) = results.
            foldLeft(FrontEnd.combineParseResult _, (ISZ[Message](), ISZ[AST.TopUnit.Program](), nameMap, typeMap))
          info = info(messages = info.messages ++ messages)
          nameMap = nm
          typeMap = tm
          programs = ps
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
          val (messages, ps, nm, tm) = results.
            foldLeft(FrontEnd.combineParseResult _, (ISZ[Message](), ISZ[AST.TopUnit.Program](), nameMap, typeMap))
          info = info(messages = info.messages ++ messages)
          nameMap = nm
          typeMap = tm
          programs = for (program <- ps if checkUriSet.contains(program.fileUriOpt.get)) yield program
      }
      val rep = Reporter.create
      val th = TypeHierarchy.build(T, TypeHierarchy(nameMap, typeMap, Poset.empty, HashMap.empty), rep)
      info = info(messages = info.messages ++ rep.messages)
      if (ops.ISZOps(info.messages).exists((m: Message) => m.isError || m.isInternalError)) {
        return (info(stop = T), F)
      }
      info = info(map = info.map + module.id ~> th)

      halt("TODO")
    }
  }

}

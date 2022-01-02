// #Sireum
/*
 Copyright (c) 2017-2022, Robby, Kansas State University
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
import org.sireum.message.Reporter
import org.sireum.project.{DependencyManager, Module, ProjectUtil}

object ModuleProcessor {
  @datatype class ProcessResult[I](imm: I, tipeStatus: B, save: B, changed: B)
  @datatype class RunResult[I](imm: I, tipeStatus: B, changed: B)
}

import ModuleProcessor._

@msig trait ModuleProcessor[I, M] {
  @pure def root: Os.Path

  @pure def module: Module

  @pure def force: B

  @pure def par: Z

  @pure def sha3: B

  @pure def followSymLink: B

  @pure def outDir: Os.Path

  @pure def fileFilter(imm: I, file: Os.Path): B

  def process(imm: I,
              mut: M,
              shouldProcess: B,
              changedFiles: HashMap[String, B],
              dm: DependencyManager,
              sourceFiles: ISZ[Os.Path],
              testSourceFiles: ISZ[Os.Path],
              reporter: Reporter): ProcessResult[I]

  def findSources(imm: I, path: Os.Path): ISZ[Os.Path] = {
    val filter = (f: Os.Path) => fileFilter(imm, f)
    return if (path.exists) for (p <- Os.Path.walk(path, F, followSymLink, filter)) yield p else ISZ()
  }

  @pure def fingerprint(imm: I, p: Os.Path): String = {
    if (sha3) {
      return st"${toInput(imm, p).fingerprint}".render
    } else {
      return s"${p.lastModified}"
    }
  }

  def toInput(imm: I, p: Os.Path): FrontEnd.Input = {
    return FrontEnd.Input(p.read, Some(p.toUri))
  }

  def run(imm: I, mut: M, dm: DependencyManager, reporter: Reporter): RunResult[I] = {
    var sourceInputs = ISZ[Os.Path]()
    var testSourceInputs = ISZ[Os.Path]()
    for (source <- ProjectUtil.moduleSources(module)) {
      sourceInputs = sourceInputs ++ findSources(imm, source)
    }
    for (testSource <- ProjectUtil.moduleTestSources(module)) {
      testSourceInputs = testSourceInputs ++ findSources(imm, testSource)
    }

    val fingerprintMap = HashMap.empty[String, String] ++ (
      if (par > 1 && sha3) ops.ISZOps(sourceInputs ++ testSourceInputs).
        mParMapCores((p: Os.Path) => (root.relativize(p).string, fingerprint(imm, p)), par)
      else for (p <- sourceInputs ++ testSourceInputs) yield (root.relativize(p).string, fingerprint(imm, p)))

    val fingerprintCache = outDir / s"${module.id}${if (sha3) ".sha3" else ""}.json"
    val (shouldProcess, changedFiles): (B, HashMap[String, B]) = if (!force && fingerprintCache.exists) {
      val jsonParser = Json.Parser.create(fingerprintCache.read)
      val map = jsonParser.parseHashMap(jsonParser.parseString _, jsonParser.parseString _)
      var cfMap = HashMap.empty[String, B]
      @strictpure def toAbs(path: String): String = {
        val p = Os.path(path)
        (if (p.isAbs) p else root / path).string
      }
      if (jsonParser.errorOpt.isEmpty) {
        var diff = F
        for (p <- map.entries) {
          val k = p._1
          fingerprintMap.get(k) match {
            case Some(v) =>
              if (p._2 != v) {
                diff = T
                cfMap = cfMap + toAbs(k) ~> T
              }
            case _ =>
          }
        }
        for (k <- fingerprintMap.keys if !map.contains(k)) {
          diff = T
          cfMap = cfMap + toAbs(k) ~> F
        }
        (diff, cfMap)
      } else {
        (T, cfMap)
      }
    } else {
      (T, HashMap.empty)
    }
    if (shouldProcess) {
      fingerprintCache.removeAll()
    }

    val ProcessResult(r, tipe, save, changed) = process(imm, mut, shouldProcess, changedFiles, dm, sourceInputs,
      testSourceInputs, reporter)
    if (save) {
      fingerprintCache.writeOver(Json.Printer.printHashMap(F, fingerprintMap, Json.Printer.printString _,
        Json.Printer.printString _).render)
    }
    return RunResult(r, tipe, changed)
  }
}


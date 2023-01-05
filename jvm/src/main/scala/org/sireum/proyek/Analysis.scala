// #Sireum
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
import org.sireum.lang.FrontEnd
import org.sireum.lang.symbol.Resolver
import org.sireum.lang.{ast => AST}
import org.sireum.lang.tipe.{PostTipeAttrChecker, TypeChecker, TypeHierarchy, TypeOutliner}
import org.sireum.logika.plugin.Plugin
import org.sireum.logika.{Config, Logika, Smt2, Smt2Impl}
import org.sireum.message.Message
import org.sireum.project._
import org.sireum.proyek.ModuleProcessor.{ProcessResult, RunResult}

object Analysis {

  @datatype class Info(val cacheInput: B,
                       val uriMap: HashMap[String, HashMap[String, FrontEnd.Input]],
                       val thMap: HashMap[String, TypeHierarchy],
                       val files: HashSMap[String, String],
                       val vfiles: ISZ[String],
                       val line: Z,
                       val all: B,
                       val verify: B,
                       val verbose: B,
                       val sanityCheck: B,
                       val config: Config,
                       val plugins: ISZ[Plugin],
                       val skipMethods: ISZ[String],
                       val skipTypes: ISZ[String])

  @record class ModuleProcessor(val root: Os.Path,
                                val module: Module,
                                val force: B,
                                val par: Z,
                                val strictAliasing: B,
                                val followSymLink: B,
                                val verbose: B,
                                val outDir: Os.Path) extends proyek.ModuleProcessor[Info, Smt2.Cache] {

    @strictpure def sha3: B = F

    @pure override def fileFilter(info: Info, file: Os.Path): B = {
      val ext = file.ext
      if (ext == "slang") {
        return T
      }
      if (ext != "scala") {
        return F
      }
      info.files.get(file.string) match {
        case Some(content) => return Proyek.firstCompactLineOps(conversions.String.toCStream(content)).contains("#Sireum")
        case _ => return Proyek.firstCompactLineOps(file.readCStream).contains("#Sireum")
      }
    }

    override def toInput(info: Info, p: Os.Path): FrontEnd.Input = {
      val uri = p.toUri
      val input: FrontEnd.Input = info.files.get(p.string) match {
        case Some(content) => FrontEnd.Input(content, Some(uri))
        case _ => FrontEnd.Input(p.read, Some(uri))
      }
      val map: HashMap[String, FrontEnd.Input] = info.uriMap.get(module.id) match {
        case Some(v) => v
        case _ => HashMap.empty
      }
      map.get(uri) match {
        case Some(oldInput) if oldInput.fingerprint == input.fingerprint =>
          if (verbose) {
            println(s"* Cache hit on parsed input $p")
          }
          return oldInput
        case _ => return input
      }
    }

    override def process(info: Info,
                         cache: Smt2.Cache,
                         shouldProcess: B,
                         changedFiles: HashSet[String],
                         dm: DependencyManager,
                         sourceFiles: ISZ[Os.Path],
                         testSourceFiles: ISZ[Os.Path],
                         reporter: message.Reporter): ProcessResult[Info] = {
      val isTipe = info.all && !info.verify

      val sourceFilePaths: ISZ[String] = for (p <- (HashSSet ++ sourceFiles ++ testSourceFiles).elements) yield p.string
      val checkFilePaths: ISZ[String] =
        if (info.all) sourceFilePaths
        else ops.ISZOps(sourceFilePaths).filter((p: String) => info.files.contains(p))

      if (!info.all && !info.config.interp && !force && checkFilePaths.isEmpty && changedFiles.isEmpty &&
        info.thMap.get(module.id).nonEmpty) {
        return ProcessResult(imm = info, tipeStatus = T, save = F, changed = F)
      }

      val (inputs, nameMap, typeMap): (ISZ[FrontEnd.Input], Resolver.NameMap, Resolver.TypeMap) = {
        if (info.verbose && sourceFilePaths.nonEmpty) {
          println(
            st"""Parsing and type outlining files:
                |${(for (p <- sourceFilePaths) yield st"* $p", "\n")}""".render
          )
        }
        var nm: Resolver.NameMap = HashSMap.empty
        var tm: Resolver.TypeMap = HashSMap.empty
        val ivyDeps = HashSet ++ module.ivyDeps
        var addBuiltIns = T
        if (ivyDeps.contains(DependencyManager.librarySharedKey)) {
          val (mnm, mtm) = FrontEnd.checkedSharedMaps
          nm = nm ++ mnm.entries
          tm = tm ++ mtm.entries
          addBuiltIns = F
        }
        if (ivyDeps.contains(DependencyManager.libraryKey)) {
          val mth = FrontEnd.checkedLibraryReporter._1.typeHierarchy
          nm = nm ++ mth.nameMap.entries
          tm = tm ++ mth.typeMap.entries
          addBuiltIns = F
        }
        if (addBuiltIns) {
          val pair = Resolver.addBuiltIns(nm, tm)
          nm = pair._1
          tm = pair._2
        }
        for (mid <- dm.project.poset.parentsOf(module.id).elements) {
          val mth = info.thMap.get(mid).get
          nm = nm ++ mth.nameMap.entries
          tm = tm ++ mth.typeMap.entries
        }
        val inputs = ops.ISZOps(for (p <- sourceFilePaths) yield toInput(info, Os.path(p)))
        if (inputs.s.isEmpty) {
          return ProcessResult(imm = info(uriMap = info.uriMap + module.id ~> HashMap.empty,
            thMap = info.thMap + module.id ~> TypeHierarchy.empty(nameMap = nm, typeMap = tm)), tipeStatus = T, save = !isTipe, changed = T)
        }
        val q = inputs.parMapFoldLeftCores((input: FrontEnd.Input) => input.parseGloballyResolve,
          FrontEnd.combineParseResult _, (ISZ[Message](), ISZ[AST.TopUnit.Program](), nm, tm), par)
        nm = q._3
        tm = q._4
        reporter.reports(q._1)
        (inputs.s, nm, tm)
      }

      val info2: Info = {
        var map = info.uriMap.get(module.id).getOrElse(HashMap.empty)
        if (info.cacheInput) {
          map = map -- (for (uri <- map.keys if !Os.uriToPath(uri).exists) yield uri)
          for (input <- inputs) {
            map = map + input.fileUriOpt.get ~> input
          }
        }
        info(uriMap = info.uriMap + module.id ~> map)
      }
      if (reporter.hasError) {
        return ProcessResult(imm = info2, tipeStatus = F, save = F, changed = T)
      }
      var th = TypeHierarchy.build(T, TypeHierarchy(nameMap, typeMap, Poset.empty, HashSMap.empty), reporter)
      if (!reporter.hasError) {
        th = TypeOutliner.checkOutline(par, strictAliasing, th, reporter)
      }
      val verifyFileUris: HashSSet[String] = if (info2.config.interp) {
        HashSSet.empty[String] ++ (for (input <- inputs) yield input.fileUriOpt.get)
      } else if (info2.all && info2.verify) {
        var vfus = HashSSet.empty[String]
        for (input <- inputs if Proyek.firstCompactLineOps(conversions.String.toCStream(input.content)).contains("#Logika")) {
          vfus = vfus + input.fileUriOpt.get
        }
        vfus
      } else {
        val vfileSet = HashSet ++ (for (f <- info2.vfiles) yield Os.path(f).toUri)
        var vfus = HashSSet.empty[String]
        for (input <- inputs if vfileSet.contains(input.fileUriOpt.get)) {
          vfus = vfus + input.fileUriOpt.get
        }
        vfus
      }
      if (reporter.hasError) {
        return ProcessResult(imm = info2, tipeStatus = F, save = F, changed = T)
      }
      if (info2.verbose && verifyFileUris.nonEmpty) {
        println(
          st"""Type checking files:
              |${(for (uri <- verifyFileUris.elements) yield st"* ${Os.uriToPath(uri)}", "\n")}""".render)
      }
      var nm: lang.symbol.Resolver.NameMap = HashSMap.empty
      var tm: lang.symbol.Resolver.TypeMap = HashSMap.empty
      if (info2.verify && verifyFileUris.nonEmpty) {
        @pure def shouldInclude(pos: message.Position): B = {
          pos.uriOpt match {
            case Some(uri) if verifyFileUris.contains(uri) =>
              val line = info2.line
              return (line <= 0) || (pos.beginLine <= line && line <= pos.endLine)
            case _ =>
          }
          return F
        }

        for (ninfo <- th.nameMap.values) {
          ninfo.posOpt match {
            case Some(pos) if shouldInclude(pos) => nm = nm + ninfo.name ~> ninfo
            case _ =>
          }
        }
        for (tinfo <- th.typeMap.values) {
          tinfo.posOpt match {
            case Some(pos) if shouldInclude(pos) => tm = tm + tinfo.name ~> tinfo
            case _ =>
          }
        }
      } else {
        nm = th.nameMap
        tm = th.typeMap
      }
      th = TypeChecker.checkComponents(par, strictAliasing, th, nm, tm, reporter)
      if (reporter.hasError) {
        return ProcessResult(imm = info2, tipeStatus = F, save = F, changed = T)
      }
      if (info2.sanityCheck) {
        for (name <- nm.keys) {
          nm = nm + name ~> th.nameMap.get(name).get
        }
        for (name <- tm.keys) {
          tm = tm + name ~> th.typeMap.get(name).get
        }
        PostTipeAttrChecker.checkNameTypeMaps(nm, tm, reporter)
        if (reporter.hasError) {
          return ProcessResult(imm = info2, tipeStatus = F, save = F, changed = T)
        }
      }
      val newFiles = info2.files -- checkFilePaths
      val info3 = info2(
        thMap = info2.thMap + module.id ~> th,
        files = newFiles
      )
      val config = info3.config
      val fileSet: HashSSet[String] = if (config.interp && !info3.all) {
        HashSSet.empty[String] ++ (for (f <- info3.vfiles) yield Os.path(f).toUri)
      } else {
        verifyFileUris
      }
      if (fileSet.isEmpty) {
        return ProcessResult(imm = info3, tipeStatus = T, save = shouldProcess, changed = T)
      }
      if (info3.verbose) {
        println(
          st"""Verifying files:
              |${(for (uri <- fileSet.elements) yield st"* ${Os.uriToPath(uri)}", "\n")}""".render)
      }
      Logika.checkTypedPrograms(
        verifyingStartTime = 0,
        fileSet = fileSet,
        config = config,
        th = th,
        smt2f = (th: TypeHierarchy) =>
          Smt2Impl.create(config.smt2Configs, Plugin.claimPlugins(info3.plugins),
            th, config.timeoutInMs, config.fpRoundingMode,
            config.charBitWidth, config.intBitWidth, config.useReal, config.simplifiedQuery, config.smt2Seq,
            config.rawInscription, config.elideEncoding, config.atLinesFresh,
            reporter.asInstanceOf[logika.Logika.Reporter]),
        cache = cache,
        reporter = reporter.asInstanceOf[logika.Logika.Reporter],
        par = par,
        plugins = info3.plugins,
        line = info3.line,
        skipMethods = info3.skipMethods,
        skipTypes = info3.skipTypes
      )
      return ProcessResult(imm = info3, tipeStatus = T, save = shouldProcess, changed = T)
    }
  }

  def run(root: Os.Path,
          outDirName: String,
          project: Project,
          dm: DependencyManager,
          cacheInput: B,
          cacheTypeHierarchy: B,
          mapBox: MBox2[HashMap[String, HashMap[String, FrontEnd.Input]], HashMap[String, TypeHierarchy]],
          config: Config,
          cache: Smt2.Cache,
          files: HashSMap[String, String],
          filesWatched: B,
          vfiles: ISZ[String],
          line: Z,
          par: Z,
          strictAliasing: B,
          followSymLink: B,
          all: B,
          verify: B,
          disableOutput: B,
          verbose: B,
          sanityCheck: B,
          plugins: ISZ[Plugin],
          skipMethods: ISZ[String],
          skipTypes: ISZ[String],
          reporter: Logika.Reporter): Z = {

    val outDir = root / outDirName / (if (all && !verify) "tipe" else "logika")
    var info = Info(
      cacheInput = cacheInput,
      uriMap = mapBox.value1,
      thMap = mapBox.value2,
      files = files,
      vfiles = vfiles,
      line = line,
      all = all,
      verify = verify,
      verbose = !disableOutput && verbose,
      sanityCheck = sanityCheck,
      config = config,
      plugins = plugins,
      skipMethods = skipMethods,
      skipTypes = skipTypes)

    var modules: ISZ[(String, B)] = for (m <- project.poset.rootNodes) yield (m, F)
    var seenModules = HashSet.empty[String]

    if (!disableOutput) {
      println()
    }

    while (modules.nonEmpty) {
      var nextModules = HashSMap.empty[String, B]
      var workModules = HashSMap.empty[String, B]
      for (p <- modules) {
        val (module, force) = p
        if ((project.poset.parentsOf(module) -- seenModules.elements).isEmpty) {
          workModules = workModules + module ~> force
        } else {
          nextModules = nextModules + module ~> force
        }
      }

      seenModules = seenModules ++ workModules.keys

      if (!disableOutput) {
        println(st"${if (info.verify) "Verifying" else "Type checking"} module${if (workModules.size == 1) "" else "s"}: ${(workModules.keys, ", ")} ...".render)
      }

      val runModule = (p: (String, B)) =>
        (p._1, ModuleProcessor(
          root = root,
          module = project.modules.get(p._1).get,
          force = p._2,
          par = par,
          strictAliasing = strictAliasing,
          followSymLink = followSymLink,
          verbose = verbose,
          outDir = outDir
        ).run(info, cache, dm, if (filesWatched) files else HashSMap.empty, reporter))

      val mvis: ISZ[(String, RunResult[Info])] =
        if (par != 1) ops.ISZOps(workModules.entries).mParMapCores(runModule, par)
        else for (module <- workModules.entries) yield runModule(module)

      if (!cacheTypeHierarchy) {
        val poset = project.poset
        mapBox.value2 = mapBox.value2 -- (for (m <- workModules.keys;
                                               mParent <- poset.parentsOf(m).elements if (poset.childrenOf(mParent) -- seenModules.elements).isEmpty) yield
          mParent)
      }

      var tipe: B = T
      for (pair <- mvis) {
        val (mid, RunResult(info2, t, changed)) = pair
        workModules = workModules + mid ~> changed
        mapBox.value1 = mapBox.value1 + mid ~> info2.uriMap.get(mid).get
        if (t) {
          mapBox.value2 = mapBox.value2 + mid ~> info2.thMap.get(mid).get
        } else {
          tipe = F
        }
        info = info(files = info.files -- (info.files.keys -- info2.files.keys))
      }
      info = info(uriMap = mapBox.value1, thMap = mapBox.value2)

      for (p <- workModules.entries; childModule <- project.poset.childrenOf(p._1).elements) {
        nextModules.get(childModule) match {
          case Some(force) => nextModules = nextModules + childModule ~> (force || p._2)
          case _ => nextModules = nextModules + childModule ~> p._2
        }
      }
      if ((all || info.files.nonEmpty) && tipe && !reporter.hasError) {
        modules = nextModules.entries
      } else {
        mapBox.value2 = mapBox.value2 -- (for (p <- nextModules.entries if p._2) yield p._1)
        modules = ISZ()
      }

      if (!disableOutput) {
        println()
      }

    }

    if (!all && files.nonEmpty && info.files.isEmpty) {
      mapBox.value2 = mapBox.value2 -- (mapBox.value2.keys -- seenModules.elements)
    }

    if (info.files.nonEmpty && !reporter.hasError) {
      reporter.warn(None(), "Proyek", st"The project configuration does not include ${(files.keys, ", ")} in the source path(s)".render)
    }

    if (reporter.hasError) {
      return -1
    } else {
      return 0
    }
  }

}

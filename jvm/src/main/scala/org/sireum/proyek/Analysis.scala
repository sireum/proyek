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
        case Some(content) => return firstCompactLineOps(conversions.String.toCStream(content)).contains("#Sireum")
        case _ => return firstCompactLineOps(file.readCStream).contains("#Sireum")
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
        case Some(oldInput) if oldInput.fingerprint == input.fingerprint => return oldInput
        case _ => return input
      }
    }

    override def process(info: Info,
                         cache: Smt2.Cache,
                         shouldProcess: B,
                         changedFiles: HashMap[String, B],
                         dm: DependencyManager,
                         sourceFiles: ISZ[Os.Path],
                         testSourceFiles: ISZ[Os.Path],
                         reporter: message.Reporter): ProcessResult[Info] = {
      if (info.verbose) {
        println()
        println(s"Checking ${module.id} ...")
      }

      val isTipe = info.all && !info.verify

      val sourceFilePaths: ISZ[String] = for (p <- sourceFiles ++ testSourceFiles) yield p.string
      val checkFilePaths: ISZ[String] =
        if (info.all) sourceFilePaths
        else ops.ISZOps(sourceFilePaths).filter((p: String) => info.files.contains(p))

      val (inputs, nameMap, typeMap, info2): (ISZ[FrontEnd.Input], Resolver.NameMap, Resolver.TypeMap, Info) =
        info.thMap.get(module.id) match {
          case Some(th) if !info.all && !force =>
            if (checkFilePaths.isEmpty && changedFiles.isEmpty) {
              return ProcessResult(imm = info, tipeStatus = T, save = F, changed = F)
            } else {
              if (info.verbose && checkFilePaths.nonEmpty) {
                println("Parsing and type outlining files:")
                for (p <- checkFilePaths) {
                  println(s"* $p")
                }
              }
              var nm = th.nameMap
              var tm = th.typeMap
              val checkFileUris = HashSSet ++ (for (p <- checkFilePaths ++ changedFiles.keys) yield Os.path(p).toUri)
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
              val inputs = ops.ISZOps(
                for (p <- checkFilePaths ++ (for (e <- changedFiles.entries if e._2) yield e._1)) yield
                  toInput(info, Os.path(p)))
              val q = inputs.parMapFoldLeftCores((input: FrontEnd.Input) => input.parseGloballyResolve,
                FrontEnd.combineParseResult _, (ISZ[Message](), ISZ[AST.TopUnit.Program](), nm, tm), par)
              reporter.reports(q._1)
              (inputs.s, q._3, q._4, info)
            }
          case _ =>
            if (info.verbose && checkFilePaths.nonEmpty) {
              if (info.verify && !info.all) {
                println("Parsing and type outlining files:")
                for (p <- sourceFilePaths) {
                  println(s"* $p")
                }
              } else {
                println("Parsing, type outlining, and type checking files:")
                for (p <- sourceFilePaths) {
                  println(s"* $p")
                }
              }
            }
            var nm: Resolver.NameMap = HashMap.empty
            var tm: Resolver.TypeMap = HashMap.empty
            if ((HashSet ++ module.ivyDeps).contains(DependencyManager.libraryKey)) {
              val mth = FrontEnd.checkedLibraryReporter._1.typeHierarchy
              nm = nm ++ mth.nameMap.entries
              tm = tm ++ mth.typeMap.entries
            }
            for (mid <- dm.project.poset.parentsOf(module.id).elements) {
              val mth = info.thMap.get(mid).get
              nm = nm ++ mth.nameMap.entries
              tm = tm ++ mth.typeMap.entries
            }
            val inputs = ops.ISZOps(for (p <- sourceFiles ++ testSourceFiles) yield toInput(info, p))
            if (inputs.s.isEmpty) {
              if (nm.isEmpty && tm.isEmpty) {
                return ProcessResult(imm = info(uriMap = info.uriMap + module.id ~> HashMap.empty,
                  thMap = info.thMap + module.id ~> TypeHierarchy.empty), tipeStatus = T, save = !isTipe, changed = T)
              }
            } else {
              val pair = Resolver.addBuiltIns(nm, tm)
              nm = pair._1
              tm = pair._2
            }
            val q = inputs.parMapFoldLeftCores((input: FrontEnd.Input) => input.parseGloballyResolve,
              FrontEnd.combineParseResult _, (ISZ[Message](), ISZ[AST.TopUnit.Program](), nm, tm), par)
            nm = q._3
            tm = q._4
            reporter.reports(q._1)
            (inputs.s, nm, tm, info)
        }

      val info3: Info = {
        var map = info2.uriMap.get(module.id).getOrElse(HashMap.empty)
        if (info2.cacheInput) {
          map = map -- (for (uri <- map.keys if !Os.uriToPath(uri).exists) yield uri)
          for (input <- inputs) {
            map = map + input.fileUriOpt.get ~> input
          }
        }
        info2(uriMap = info2.uriMap + module.id ~> map)
      }
      if (reporter.hasError) {
        return ProcessResult(imm = info3, tipeStatus = F, save = F, changed = T)
      }
      var th = TypeHierarchy.build(T, TypeHierarchy(nameMap, typeMap, Poset.empty, HashMap.empty), reporter)
      if (!reporter.hasError) {
        th = TypeOutliner.checkOutline(par, strictAliasing, th, reporter)
      }
      val verifyFileUris: HashSSet[String] = if (info.all) {
        var vfus = HashSSet.empty[String]
        for (input <- inputs if firstCompactLineOps(conversions.String.toCStream(input.content)).contains("#Logika")) {
          vfus = vfus + input.fileUriOpt.get
        }
        vfus
      } else {
        val vfileSet = HashSet ++ (if (info.vfiles.isEmpty) info.files.keys else info.vfiles)
        var vfus = HashSSet.empty[String]
        for (p <- checkFilePaths if vfileSet.contains(p)) {
          vfus = vfus + Os.path(p).toUri
        }
        vfus
      }
      if (!reporter.hasError) {
        if (info.verify && info.verbose) {
          if (info.all) {
            if (verifyFileUris.nonEmpty) {
              println()
              println("Verifying files:")
              for (uri <- verifyFileUris.elements) {
                println(s"* ${Os.uriToPath(uri)}")
              }
            }
          } else {
            if (verifyFileUris.nonEmpty) {
              println()
              println("Type checking and verifying files:")
              for (uri <- verifyFileUris.elements) {
                println(s"* ${Os.uriToPath(uri)}")
              }
            }
          }
        }
        var nm = HashMap.empty[ISZ[String], lang.symbol.Info]
        var tm = HashMap.empty[ISZ[String], lang.symbol.TypeInfo]
        if (info.all || isTipe) {
          nm = th.nameMap
          tm = th.typeMap
        } else {
          @pure def shouldInclude(pos: message.Position): B = {
            pos.uriOpt match {
              case Some(uri) if verifyFileUris.contains(uri) =>
                val line = info.line
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
        }
        th = TypeChecker.checkComponents(par, strictAliasing, th, nm, tm, reporter)
        if (reporter.hasError) {
          return ProcessResult(imm = info3, tipeStatus = F, save = F, changed = T)
        }
        if (info.sanityCheck) {
          for (name <- nm.keys) {
            nm = nm + name ~> th.nameMap.get(name).get
          }
          for (name <- tm.keys) {
            tm = tm + name ~> th.typeMap.get(name).get
          }
          PostTipeAttrChecker.checkNameTypeMaps(nm, tm, reporter)
          if (reporter.hasError) {
            return ProcessResult(imm = info3, tipeStatus = F, save = F, changed = T)
          }
        }
      }
      val newFiles = info3.files -- checkFilePaths
      val info4 = info3(
        thMap = info3.thMap + module.id ~> th,
        files = newFiles
      )
      if (!info.verify || verifyFileUris.isEmpty) {
        return ProcessResult(imm = info4, tipeStatus = T, save = !isTipe && shouldProcess, changed = T)
      }
      val config = info.config
      Logika.checkTypedPrograms(
        verifyingStartTime = 0,
        fileSet = verifyFileUris,
        config = config,
        th = th,
        smt2f = (th: TypeHierarchy) =>
          Smt2Impl.create(config.smt2Configs, th, config.timeoutInMs, config.cvc4RLimit, config.fpRoundingMode,
            config.charBitWidth, config.intBitWidth, config.useReal, config.simplifiedQuery,
            reporter.asInstanceOf[logika.Logika.Reporter]),
        cache = cache,
        reporter = reporter.asInstanceOf[logika.Logika.Reporter],
        par = par,
        plugins = info.plugins,
        line = info.line,
        skipMethods = info.skipMethods,
        skipTypes = info.skipTypes
      )
      return ProcessResult(imm = info4, tipeStatus = T, save = !isTipe && shouldProcess, changed = T)
    }
  }

  def run(root: Os.Path,
          project: Project,
          dm: DependencyManager,
          cacheInput: B,
          cacheTypeHierarchy: B,
          mapBox: MBox2[HashMap[String, HashMap[String, FrontEnd.Input]], HashMap[String, TypeHierarchy]],
          config: Config,
          cache: Smt2.Cache,
          files: HashSMap[String, String],
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

    val outDir = root / "out" / (if (all && !verify) "tipe" else "logika")
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

    if (!disableOutput && !verbose) {
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

      if (!disableOutput && !verbose) {
        println(st"${if (info.verify) "Verifying" else "Type checking"} module${if (workModules.size === 1) "" else "s"}: ${(workModules.keys, ", ")} ...".render)
      }

      val runModule = (p: (String, B)) =>
        (p._1, ModuleProcessor(
          root = root,
          module = project.modules.get(p._1).get,
          force = p._2,
          par = par,
          strictAliasing = strictAliasing,
          followSymLink = followSymLink,
          outDir = outDir
        ).run(info, cache, dm, reporter))

      val mvis: ISZ[(String, RunResult[Info])] =
        if (par != 1 && !verbose) ops.ISZOps(workModules.entries).mParMapCores(runModule, par)
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

      if ((all || info.files.nonEmpty) && tipe && !reporter.hasError) {
        for (p <- workModules.entries; childModule <- project.poset.childrenOf(p._1).elements) {
          nextModules.get(childModule) match {
            case Some(force) => nextModules = nextModules + childModule ~> (force || p._2)
            case _ => nextModules = nextModules + childModule ~> p._2
          }
        }
        modules = nextModules.entries
      } else {
        modules = ISZ()
      }

      if (!disableOutput && !verbose) {
        println()
      }

    }

    if (!all && files.nonEmpty && info.files.isEmpty) {
      mapBox.value2 = mapBox.value2 -- (mapBox.value2.keys -- seenModules.elements)
    }

    return if (reporter.hasError) -1 else 0
  }

  @pure def firstCompactLineOps(cs: Jen[C]): ops.StringOps = {
    var cis = ISZ[C]()
    for (c <- cs.takeWhile((c: C) => c != '\n') if !c.isWhitespace) {
      cis = cis :+ c
    }
    return ops.StringOps(conversions.String.fromCis(cis))
  }

}

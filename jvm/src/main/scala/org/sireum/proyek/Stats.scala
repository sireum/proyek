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
import org.sireum.lang.symbol.Resolver.{NameMap, TypeMap}
import org.sireum.lang.symbol.TypeInfo
import org.sireum.lang.{FrontEnd, ast => AST}
import org.sireum.message.Message
import org.sireum.project._
import org.sireum.proyek.ModuleProcessor.{ProcessResult, RunResult}

object Stats {

  object Info {
    @strictpure def create: Info = Info(0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
      0, 0, 0, 0, 0, 0, 0, 0, 0)
  }

  @datatype class Info(val numOfFiles: Z,
                       val numOfLines: Z,
                       val numOfStmts: Z,
                       val numOfExps: Z,
                       val numOfPatterns: Z,
                       val numOfSubZs: Z,
                       val numOfEnums: Z,
                       val numOfSigs: Z,
                       val numOfMSigs: Z,
                       val numOfDatatypeTraits: Z,
                       val numOfRecordTraits: Z,
                       val numOfDatatypeClasses: Z,
                       val numOfRecordClasses: Z,
                       val numOfObjects: Z,
                       val numOfObjectFields: Z,
                       val numOfObjectSpecFields: Z,
                       val numOfObjectMethods: Z,
                       val numOfObjectSpecMethods: Z,
                       val numOfObjectInvs: Z,
                       val numOfInstanceFields: Z,
                       val numOfInstanceSpecFields: Z,
                       val numOfInstanceMethods: Z,
                       val numOfInstanceSpecMethods: Z,
                       val numOfInstanceInvs: Z,
                       val numOfNestedMethods: Z,
                       val numOfExts: Z,
                       val numOfExtFields: Z,
                       val numOfExtSpecFields: Z,
                       val numOfExtMethods: Z,
                       val numOfExtSpecMethods: Z,
                       val numOfExtInvs: Z,
                       val numOfTypeAliases: Z,
                       val numOfScalaFiles: Z,
                       val numOfScalaLines: Z,
                       val numOfJavaFiles: Z,
                       val numOfJavaLines: Z) {

    @strictpure def combine(other: Info): Info = Info(
      numOfFiles = numOfFiles + other.numOfFiles,
      numOfLines = numOfLines + other.numOfLines,
      numOfStmts = numOfStmts + other.numOfStmts,
      numOfExps = numOfExps + other.numOfExps,
      numOfPatterns = numOfPatterns + other.numOfPatterns,
      numOfSubZs = numOfSubZs + other.numOfSubZs,
      numOfEnums = numOfEnums + other.numOfEnums,
      numOfSigs = numOfSigs + other.numOfSigs,
      numOfMSigs = numOfMSigs + other.numOfMSigs,
      numOfDatatypeTraits = numOfDatatypeTraits + other.numOfDatatypeTraits,
      numOfRecordTraits = numOfRecordTraits + other.numOfRecordTraits,
      numOfDatatypeClasses = numOfDatatypeClasses + other.numOfDatatypeClasses,
      numOfRecordClasses = numOfRecordClasses + other.numOfRecordClasses,
      numOfObjects = numOfObjects + other.numOfObjects,
      numOfObjectFields = numOfObjectFields + other.numOfObjectFields,
      numOfObjectSpecFields = numOfObjectSpecFields + other.numOfObjectSpecFields,
      numOfObjectMethods = numOfObjectMethods + other.numOfObjectMethods,
      numOfObjectSpecMethods = numOfObjectSpecMethods + other.numOfObjectSpecMethods,
      numOfObjectInvs = numOfObjectInvs + other.numOfObjectInvs,
      numOfInstanceFields = numOfInstanceFields + other.numOfInstanceFields,
      numOfInstanceSpecFields = numOfInstanceSpecFields + other.numOfInstanceSpecFields,
      numOfInstanceMethods = numOfInstanceMethods + other.numOfInstanceMethods,
      numOfInstanceSpecMethods = numOfInstanceSpecMethods + other.numOfInstanceSpecMethods,
      numOfInstanceInvs = numOfInstanceInvs + other.numOfInstanceInvs,
      numOfNestedMethods = numOfNestedMethods + other.numOfNestedMethods,
      numOfExts = numOfExts + other.numOfExts,
      numOfExtFields = numOfExtFields + other.numOfExtFields,
      numOfExtSpecFields = numOfExtSpecFields + other.numOfExtSpecFields,
      numOfExtMethods = numOfExtMethods + other.numOfExtMethods,
      numOfExtSpecMethods = numOfExtSpecMethods + other.numOfExtSpecMethods,
      numOfExtInvs = numOfExtInvs + other.numOfExtInvs,
      numOfTypeAliases = numOfTypeAliases + other.numOfTypeAliases,
      numOfScalaFiles = numOfScalaFiles + other.numOfScalaFiles,
      numOfScalaLines = numOfScalaLines + other.numOfScalaLines,
      numOfJavaFiles = numOfJavaFiles + other.numOfJavaFiles,
      numOfJavaLines = numOfJavaLines + other.numOfJavaLines)

    @strictpure def numOfTypes: Z = numOfSubZs + numOfEnums + numOfSigs + numOfMSigs + numOfDatatypeTraits +
      numOfDatatypeClasses + numOfRecordTraits + numOfRecordClasses + numOfObjects + numOfExts + numOfTypeAliases

    @strictpure def numOfFields: Z = numOfObjectFields + numOfInstanceFields + numOfExtFields

    @strictpure def numOfMethods: Z = numOfObjectMethods + numOfInstanceMethods + numOfExtMethods + numOfNestedMethods

    @strictpure def numOfSpecFields: Z = numOfObjectSpecFields + numOfInstanceSpecFields + numOfExtSpecFields

    @strictpure def numOfSpecMethods: Z = numOfObjectSpecMethods + numOfInstanceMethods + numOfExtSpecMethods

    @strictpure def numOfInvs: Z = numOfObjectInvs + numOfInstanceInvs + numOfExtInvs
  }

  @enum object Context {
    "Ext"
    "Instance"
    "Object"
  }

  @record class StatsCollector(var context: Context.Type, var info: Info) extends AST.MTransformer {
    def processStmts(stmts: ISZ[AST.Stmt]): Unit = {
      for (stmt <- stmts) {
        stmt match {
          case stmt: AST.Stmt.Var =>
            context match {
              case Context.Ext => info = info(numOfExtFields = info.numOfExtFields + 1)
              case Context.Instance => info = info(numOfInstanceFields = info.numOfInstanceFields + 1)
              case Context.Object => info = info(numOfObjectFields = info.numOfObjectFields + 1)
            }
            stmt.initOpt match {
              case Some(init) => transformAssignExp(init)
              case _ =>
            }
          case stmt: AST.Stmt.Method =>
            context match {
              case Context.Ext => info = info(numOfExtMethods = info.numOfExtMethods + 1)
              case Context.Instance => info = info(numOfInstanceMethods = info.numOfInstanceMethods + 1)
              case Context.Object => info = info(numOfObjectMethods = info.numOfObjectMethods + 1)
            }
            stmt.bodyOpt match {
              case Some(body) => transformBody(body)
              case _ =>
            }
          case _: AST.Stmt.SpecVar =>
            context match {
              case Context.Ext => info = info(numOfExtSpecFields = info.numOfExtSpecFields + 1)
              case Context.Instance => info = info(numOfInstanceSpecFields = info.numOfInstanceSpecFields + 1)
              case Context.Object => info = info(numOfObjectSpecFields = info.numOfObjectSpecFields + 1)
            }
          case _: AST.Stmt.SpecMethod =>
            context match {
              case Context.Ext => info = info(numOfExtSpecMethods = info.numOfExtSpecMethods + 1)
              case Context.Instance => info = info(numOfInstanceSpecMethods = info.numOfInstanceSpecMethods + 1)
              case Context.Object => info = info(numOfObjectSpecMethods = info.numOfObjectSpecMethods + 1)
            }
          case _: AST.Stmt.Inv =>
            context match {
              case Context.Ext => info = info(numOfExtInvs = info.numOfExtInvs + 1)
              case Context.Instance => info = info(numOfInstanceInvs = info.numOfInstanceInvs + 1)
              case Context.Object => info = info(numOfObjectInvs = info.numOfObjectInvs + 1)
            }
          case _ =>
        }
      }
    }

    override def postStmt(o: AST.Stmt): MOption[AST.Stmt] = {
      o match {
        case _: AST.Stmt.Method =>
        case _ => info = info(numOfStmts = info.numOfStmts + 1)
      }
      return MNone()
    }

    override def postExp(o: AST.Exp): MOption[AST.Exp] = {
      info = info(numOfExps = info.numOfExps + 1)
      return MNone()
    }

    override def postPattern(o: AST.Pattern): MOption[AST.Pattern] = {
      info = info(numOfPatterns = info.numOfPatterns + 1)
      return MNone()
    }

    override def postStmtMethod(o: AST.Stmt.Method): MOption[AST.Stmt] = {
      info = info(numOfNestedMethods = info.numOfNestedMethods + 1)
      return MNone()
    }
  }

  @record class ModuleProcessor(val root: Os.Path,
                                val module: Module,
                                val par: Z,
                                val strictAliasing: B,
                                val followSymLink: B,
                                val outDir: Os.Path) extends proyek.ModuleProcessor[HashSMap[String, Info], B] {

    @strictpure def force: B = T
    @strictpure def sha3: B = F

    @pure override def fileFilter(infoMap: HashSMap[String, Info], file: Os.Path): B = {
      val ext = file.ext
      return ext == "scala" || ext == "java" || ext == "slang"
    }

    override def process(infoMap: HashSMap[String, Info],
                         b: B,
                         shouldProcess: B,
                         changedFiles: HashMap[String, B],
                         dm: DependencyManager,
                         sourceFiles: ISZ[Os.Path],
                         testSourceFiles: ISZ[Os.Path],
                         reporter: message.Reporter): ProcessResult[HashSMap[String, Info]] = {
      var javaFiles = ISZ[Os.Path]()
      var scalaFiles = ISZ[Os.Path]()
      var slangFiles = ISZ[Os.Path]()
      for (p <- (HashSSet ++ sourceFiles ++ testSourceFiles).elements if p.isFile) {
        p.ext match {
          case string"scala" =>
            if (Proyek.firstCompactLineOps(p.readCStream).contains("#Sireum")) {
              slangFiles = slangFiles :+ p
            } else {
              scalaFiles = scalaFiles :+ p
            }
          case string"slang" => slangFiles = slangFiles :+ p
          case string"java" => javaFiles = javaFiles :+ p
        }
      }
      val (inputs, nameMap, typeMap): (ISZ[FrontEnd.Input], NameMap, TypeMap) = {
        val inputs = ops.ISZOps(for (p <- slangFiles) yield toInput(infoMap, p))
        var nm: NameMap = HashSMap.empty
        var tm: TypeMap = HashSMap.empty
        val q = inputs.parMapFoldLeftCores((input: FrontEnd.Input) => input.parseGloballyResolve,
          FrontEnd.combineParseResult _, (ISZ[Message](), ISZ[AST.TopUnit.Program](), nm, tm), par)
        nm = q._3
        tm = q._4
        reporter.reports(q._1)
        (inputs.s, nm, tm)
      }
      val sc = StatsCollector(Context.Object, Info.create)
      var infoMap2 = infoMap + module.id ~> sc.info
      if (reporter.hasError) {
        return ProcessResult(imm = infoMap, tipeStatus = F, save = F, changed = T)
      }
      sc.info = sc.info(numOfFiles = inputs.size)
      @strictpure def numOfLines(content: String): Z = ops.StringOps(content).split((c: C) => c == '\n').size
      for (input <- inputs) {
        sc.info = sc.info(numOfLines = sc.info.numOfLines + numOfLines(input.content))
      }
      for (p <- javaFiles) {
        sc.info = sc.info(numOfJavaFiles = sc.info.numOfJavaFiles + 1,
          numOfJavaLines = sc.info.numOfJavaLines + numOfLines(p.read))
      }
      for (p <- scalaFiles) {
        sc.info = sc.info(numOfScalaFiles = sc.info.numOfScalaFiles + 1,
          numOfScalaLines = sc.info.numOfScalaLines + numOfLines(p.read))
      }
      for (info <- nameMap.values) {
        info match {
          case info: lang.symbol.Info.Object =>
            info.ast.extNameOpt match {
              case Some(_) => sc.context = Context.Ext
              case _ => sc.context = Context.Object
            }
            sc.processStmts(info.ast.stmts)
            sc.transformStmt(info.ast)
          case _ =>
        }
      }
      for (info <- typeMap.values) {
        info match {
          case _: TypeInfo.SubZ => sc.info = sc.info(numOfSubZs = sc.info.numOfSubZs + 1)
          case _: TypeInfo.Enum => sc.info = sc.info(numOfEnums = sc.info.numOfEnums + 1)
          case _: TypeInfo.TypeAlias => sc.info = sc.info(numOfTypeAliases = sc.info.numOfTypeAliases + 1)
          case info: TypeInfo.Adt =>
            sc.context = Context.Instance
            if (info.ast.isRoot) {
              if (info.ast.isDatatype) {
                sc.info = sc.info(numOfDatatypeTraits = sc.info.numOfDatatypeTraits + 1)
              } else {
                sc.info = sc.info(numOfRecordTraits = sc.info.numOfRecordTraits + 1)
              }
            } else {
              if (info.ast.isDatatype) {
                sc.info = sc.info(numOfDatatypeClasses = sc.info.numOfDatatypeClasses + 1)
              } else {
                sc.info = sc.info(numOfRecordClasses = sc.info.numOfRecordClasses + 1)
              }
            }
            sc.info = sc.info(numOfInstanceFields = sc.info.numOfInstanceFields + info.ast.params.size)
            sc.processStmts(info.ast.stmts)
          case info: TypeInfo.Sig =>
            sc.context = Context.Instance
            if (info.ast.isImmutable) {
              sc.info = sc.info(numOfSigs = sc.info.numOfSigs + 1)
            } else {
              sc.info = sc.info(numOfMSigs = sc.info.numOfMSigs + 1)
            }
            sc.processStmts(info.ast.stmts)
          case _ =>
        }
      }
      infoMap2 = infoMap + module.id ~> sc.info
      return ProcessResult(imm = infoMap2, tipeStatus = F, save = F, changed = T)
    }
  }

  def run(root: Os.Path,
          project: Project,
          dm: DependencyManager,
          par: Z,
          strictAliasing: B,
          followSymLink: B,
          output: Os.Path,
          reporter: message.Reporter): Z = {

    val outDir = root / "out" / "stats"
    var infoMap = HashSMap.empty[String, Info]

    var modules: ISZ[String] = project.poset.rootNodes
    var seenModules = HashSet.empty[String]

    while (modules.nonEmpty) {
      var nextModules = HashSSet.empty[String]
      var workModules = HashSSet.empty[String]
      for (module <- modules) {
        if ((project.poset.parentsOf(module) -- seenModules.elements).isEmpty) {
          workModules = workModules + module
        } else {
          nextModules = nextModules + module
        }
      }

      seenModules = seenModules ++ workModules.elements

      println(st"Analyzing module${if (workModules.size > 1) "s" else ""}: ${(workModules.elements, ", ")} ...".render)

      val runModule = (mid: String) =>
        (mid, ModuleProcessor(
          root = root,
          module = project.modules.get(mid).get,
          par = par,
          strictAliasing = strictAliasing,
          followSymLink = followSymLink,
          outDir = outDir
        ).run(infoMap, F, dm, reporter))

      val mvis: ISZ[(String, RunResult[HashSMap[String, Info]])] =
        if (par != 1) ops.ISZOps(workModules.elements).mParMapCores(runModule, par)
        else for (module <- workModules.elements) yield runModule(module)

      for (pair <- mvis) {
        val (mid, RunResult(infoMap2, _, _)) = pair
        infoMap = infoMap + mid ~> infoMap2.get(mid).get
      }

      println()

      nextModules = HashSSet ++
        (for (m <- workModules.elements; childModule <- project.poset.childrenOf(m).elements) yield childModule)

      if (!reporter.hasError) {
        modules = nextModules.elements
      } else {
        modules = ISZ()
      }
    }
    if (reporter.hasError) {
      println()
      reporter.printMessages()
      return -1
    }
    @strictpure def combine(info1: Info, info2: Info): Info = info1.combine(info2)
    @pure def info2ST(title: String, info: Info): ST = {
      val slangST: ST = if (info.numOfFiles == 0) st"0,-,-,-,-,-,-,-"
      else st"${info.numOfFiles},${info.numOfLines},${info.numOfTypes},${info.numOfFields},${info.numOfMethods},${info.numOfStmts},${info.numOfExps},${info.numOfPatterns}"
      val scalaST: ST = if (info.numOfScalaFiles == 0) st"0,-" else st"${info.numOfScalaFiles},${info.numOfScalaLines}"
      val javaST: ST = if (info.numOfJavaFiles == 0) st"0,-" else st"${info.numOfJavaFiles},${info.numOfJavaLines}"
      return st"$title,$slangST,$scalaST,$javaST,${info.numOfFiles + info.numOfScalaFiles + info.numOfJavaFiles},${info.numOfLines + info.numOfScalaLines + info.numOfJavaLines}"
    }
    output.writeOver(
      st"""${(ISZ[String]("Module", "Slang Files", "Lines", "Types", "Fields", "Methods", "Statements", "Expressions", "Patterns", "Scala Files", "Lines", "Java Files", "Lines", "Total Files", "Lines"), ",")}
          |${(for (p <- infoMap.entries) yield info2ST(p._1, p._2), "\n")}
          |${(info2ST(s"Total: ${infoMap.size} module${if (infoMap.size > 1) "s" else ""}", ops.ISZOps(infoMap.values).foldLeft(combine _, Info.create)), ",")}""".render)
    println(s"Wrote $output")
    return 0
  }

}

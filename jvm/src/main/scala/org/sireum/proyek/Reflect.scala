// #Sireum
/*
 Copyright (c) 2017-2024, Robby, Kansas State University
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
import org.sireum.lang.symbol.{Info, TypeInfo}
import org.sireum.lang.symbol.Resolver.{NameMap, TypeMap}

object Reflect {
  val maxParams: Z = 247
  val anyName: String = "Any"
  val anyRefName: String = "AnyRef"

  def gen(packageNameOpt: Option[String], className: String, outputOpt: Option[Os.Path], nameMap: NameMap,
          typeMap: TypeMap, includedPackages: HashSet[ISZ[String]], excludedPackages: HashSet[ISZ[String]],
          included: HashSet[ISZ[String]], excluded: HashSet[ISZ[String]], licenseOpt: Option[String]): Unit = {

    @strictpure def methodMapST(i: Z, puts: ISZ[ST]): ST = {
      val argTypes: ISZ[ST] = for (_ <- 0 until i) yield st" => $anyName"
      st"""private lazy val method${i}Map: Long2ObjectOpenHashMap[Option[$anyRefName]$argTypes => $anyName] = {
          |  val r = new Long2ObjectOpenHashMap[Option[$anyRefName]$argTypes => $anyName](${puts.size})
          |  ${(puts, "\n")}
          |  r
          |}"""
    }

    @strictpure def invokeST(i: Z): ST = {
      val ts: ISZ[ST] = for (j <- 0 until i) yield st", T${j + 1}"
      val argDecls: ISZ[ST] = for (j <- 0 until i) yield st", o${j + 1}: T${j + 1}"
      val args: ISZ[ST] = for (j <- 0 until i) yield st"(o${j + 1})"
      st"""override def invoke$i[T$ts, R](owner: String, name: String, rOpt: Option[T]$argDecls): R = {
          |  val f = method${i}Map.get(methodKey(rOpt.isEmpty, owner, name).value)
          |  if (f == null) {
          |    illegalReflection("Unavailable", rOpt.isEmpty, owner, name)
          |  }
          |  val r: R = X(f(X(rOpt))$args)
          |  if (r == null) {
          |    illegalReflection("Invalid", rOpt.isEmpty, owner, name)
          |  }
          |  r
          |}"""
    }

    @strictpure def bool(b : B): String = if (b) "T" else "F"

    @strictpure def infoFieldST(isInObject: B, kind: Reflection.Field.Kind.Type, stmt: AST.Stmt.Var): ST =
      st"""Field(isInObject = $isInObject, isVal = ${bool(stmt.isVal)}, kind = Field.Kind.$kind, name = "${ops.StringOps(stmt.id.value).escapeST}")"""

    @strictpure def infoMethodST(isInObject: B, owner: ISZ[String], sig: AST.MethodSig): ST = {
      assert(sig.params.size <= maxParams, st"${sig.params.size} > $maxParams: ${(owner, ".")}${if (isInObject) "." else "#"}${sig.id.value}".render)
      st"""Method(isInObject = $isInObject, isByName = ${bool(!sig.hasParams)}, name = "${ops.StringOps(sig.id.value).escapeST}", params = ISZ(${(for (p <- sig.params) yield st"\"${ops.StringOps(p.id.value).escapeST}\"", ", ")}))"""
    }

    @strictpure def quote(id: String): ST = if (ops.StringOps(id).isJavaId) st"$id" else st"`${ops.StringOps(id).escapeST}`"

    @strictpure def anyTypeVar(t: AST.Typed): ST = t match {
      case t: AST.Typed.Tuple => st"(${(for (t2 <- t.args) yield anyTypeVar(t2), ", ")})"
      case t: AST.Typed.Name =>
        if (t == AST.Typed.unit) st"Unit"
        else if (t == AST.Typed.nothing) st"Nothing"
        else st"${(t.ids, ".")}${if (t.args.isEmpty) st"" else st"[${(for (t2 <- t.args) yield anyTypeVar(t2), ", ")}]"}"
      case t: AST.Typed.Fun => st"((${(for (t2 <- t.args) yield anyTypeVar(t2), ", ")}) => ${anyTypeVar(t.ret)})"
      case _: AST.Typed.TypeVar => st"_"
      case _ => halt("Infeasible")
    }

    @strictpure def cast(o: ST, t: AST.Typed): ST = {
      val tST = anyTypeVar(t)
      if (isByName(t)) st"X(X[$tST]($o)())" else st"X($o)"
    }

    @strictpure def isByName(t: AST.Typed): B = {
      t match {
        case t: AST.Typed.Fun => t.isByName
        case _ => F
      }
    }

    @strictpure def methodSTF(isInObject: B, owner: ISZ[String], ownerTypeVars: ISZ[AST.TypeParam], id: String, hasParams: B, paramTypes: ISZ[AST.Typed], f: ST => ST): ST = {
      val ps: ST = if (!hasParams) st"" else st"${for (i <- paramTypes.indices) yield st" => (o${i + 1}: $anyName)"}"
      val args: ST = if (!hasParams) {
        st""
      } else {
        st"(${(for (i <- paramTypes.indices) yield st"${cast(st"o${i + 1}", paramTypes(i))}", ", ")})"
      }
      val nameEscape = st"${(for (n <- owner) yield ops.StringOps(n).escapeST, ".")}"
      val idEscape = ops.StringOps(id).escapeST
      val name = st"${(owner, ".")}".render
      val ownerAccess: ST = if (isInObject) st"${(for (n <- owner) yield quote(n), ".")}" else
        st"X[${(for (n <- owner) yield quote(n), ".")}${if (ownerTypeVars.isEmpty) st"" else
          st"[${(for (_ <- ownerTypeVars.indices) yield "_", ", ")}]"}](${if (!hasParams || paramTypes.isEmpty) "_" else "r"})"
      st"""r.put(0x${Reflection.methodKey(isInObject, name, id)}L, ${if (isInObject) st"_$ps => " else if (!hasParams || paramTypes.isEmpty) st"" else st"r$ps => "}${f(st"$ownerAccess.${quote(id)}$args")}) // methodKey(${bool(isInObject)}, "$nameEscape", "$idEscape").value"""
    }

    @strictpure def methodST(isInObject: B, owner: ISZ[String], ownerTypeVars: ISZ[AST.TypeParam], id: String, hasParams: B, paramTypes: ISZ[AST.Typed]): ST =
      methodSTF(isInObject, owner, ownerTypeVars, id, hasParams, paramTypes, (o: ST) => o)

    @strictpure def typesOfParams(ps: ISZ[AST.Param]): ISZ[AST.Typed] = for (p <- ps) yield p.tipe.typedOpt.get

    var infos = ISZ[ST]()
    var others = ISZ[ST]()
    var putss = ISZ.create(maxParams + 1, ISZ[ST]())

    @pure def shouldInclude(name: ISZ[String]): B = {
      if (excluded.contains(name)) {
        return F
      }
      for (xs <- excluded.elements) {
        if (xs.size < name.size) {
          var found = T
          for (i <- xs.indices) {
            if (xs(i) != name(i)) {
              found = F
            }
          }
          if (found) {
            return F
          }
        }
      }
      if (included.contains(name)) {
        return T
      }
      for (is <- included.elements) {
        if (is.size < name.size) {
          var found = T
          for (i <- is.indices) {
            if (is(i) != name(i)) {
              found = F
            }
          }
          if (found) {
            return T
          }
        }
      }
      var n = name
      while (n.nonEmpty && nameMap.get(n).map((info: Info) => !info.isInstanceOf[Info.Package]).getOrElse(T)) {
        n = ops.ISZOps(n).dropRight(1)
      }
      return (includedPackages.isEmpty || includedPackages.contains(n)) && !excludedPackages.contains(n)
    }

    def genObject(info: Info.Object): Unit = {
      if (!shouldInclude(info.name) || info.isSynthetic) {
        return
      }
      var kind: Reflection.Kind.Type = Reflection.Kind.Object
      var fields = ISZ[ST]()
      var methods = ISZ[ST]()
      for (stmt <- info.ast.stmts) {
        stmt match {
          case stmt: AST.Stmt.Var =>
            fields = fields :+ infoFieldST(T, Reflection.Field.Kind.Normal, stmt)
            putss = putss(0 ~> (putss(0) :+ methodST(T, info.name, ISZ(), stmt.id.value, F, ISZ())))
            if (!stmt.isVal) {
              putss = putss(1 ~> (putss(1) :+
                methodST(T, info.name, ISZ(), s"${stmt.id.value}_=", T, ISZ(stmt.tipeOpt.get.typedOpt.get))))
            }
          case stmt: AST.Stmt.Method if stmt.sig.params.size <= maxParams =>
            methods = methods :+ infoMethodST(T, info.name, stmt.sig)
            putss = putss(stmt.sig.params.size ~> (putss(stmt.sig.params.size) :+
              methodST(T, info.name, ISZ(), stmt.sig.id.value, stmt.sig.hasParams, typesOfParams(stmt.sig.params))))
          case stmt: AST.Stmt.ExtMethod if stmt.sig.params.size <= maxParams =>
            kind = Reflection.Kind.Ext
            methods = methods :+ infoMethodST(T, info.name, stmt.sig)
            putss = putss(stmt.sig.params.size ~> (putss(stmt.sig.params.size) :+
              methodST(T, info.name, ISZ(), stmt.sig.id.value, stmt.sig.hasParams, typesOfParams(stmt.sig.params))))
          case _ =>
        }
      }
      val name = st"${(for (n <- info.name) yield ops.StringOps(n).escapeST, ".")}".render
      val nameValue = Reflection.objectOrTypeKey(name)
      val fieldsST: ST = if (fields.isEmpty) st"fields = ISZ()" else
        st"""fields = ISZ(
            |  ${(fields, ",\n")}
            |)"""
      val methodsST: ST = if (methods.isEmpty) st"methods = ISZ()" else
        st"""methods = ISZ(
            |  ${(methods, ",\n")}
            |)"""
      others = others :+
        st"""def info${infos.size} = Info( // ${(info.name, ".")}
            |  kind = Kind.$kind,
            |  $fieldsST,
            |  $methodsST
            |)"""
      infos = infos :+ st"""r.put(0x$nameValue, info${infos.size}) // objectOrTypeKey("$name").value"""
    }

    def genSig(info: TypeInfo.Sig): Unit = {
      if (!shouldInclude(info.name)) {
        return
      }
      val kind: Reflection.Kind.Type = if (info.ast.isImmutable) Reflection.Kind.Sig else Reflection.Kind.MSig
      var methods = ISZ[ST]()
      for (m <- info.methods.values if m.ast.sig.params.size <= maxParams) {
        methods = methods :+ infoMethodST(F, info.name, m.ast.sig)
        putss = putss(m.ast.sig.params.size ~> (putss(m.ast.sig.params.size) :+
          methodST(F, info.name, info.ast.typeParams, m.ast.sig.id.value, m.ast.sig.hasParams, typesOfParams(m.ast.sig.params))))
      }
      val name = st"${(for (n <- info.name) yield ops.StringOps(n).escapeST, ".")}".render
      val nameValue = Reflection.objectOrTypeKey(name)
      val fieldsST = st"fields = ISZ()"
      val methodsST: ST = if (methods.isEmpty) st"methods = ISZ()" else
        st"""methods = ISZ(
            |  ${(methods, ",\n")}
            |)"""
      others = others :+
        st"""def info${infos.size} = Info( // ${(info.name, ".")}
            |  kind = Kind.$kind,
            |  $fieldsST,
            |  $methodsST
            |)"""
      infos = infos :+ st"""r.put(0x$nameValue, info${infos.size}) // objectOrTypeKey("$name").value"""
    }

    def genAdt(info: TypeInfo.Adt): Unit = {
      if (!shouldInclude(info.name)) {
        return
      }
      val kind: Reflection.Kind.Type = {
        (info.ast.isRoot, info.ast.isDatatype) match {
          case (T, T) => Reflection.Kind.DatatypeTrait
          case (T, F) => Reflection.Kind.RecordTrait
          case (F, T) => Reflection.Kind.DatatypeClass
          case (F, F) => Reflection.Kind.RecordClass
        }
      }
      val params = HashMap ++ (for (p <- info.ast.params) yield
        (p.id.value, if (p.isHidden) Reflection.Field.Kind.Hidden else Reflection.Field.Kind.Param))
      var fields = ISZ[ST]()
      var methods = ISZ[ST]()
      if (!info.ast.isRoot) {
        putss = putss(params.size ~> (putss(params.size) :+
          methodST(T, info.name, info.ast.typeParams, Reflection.constructorName, T, for (p <- info.ast.params) yield p.tipe.typedOpt.get)))
        val os: ISZ[ST] = for (i <- info.ast.params.indices if !info.ast.params(i).isHidden) yield st"o$i"
        if (os.size <= 22) {
          val osST: ST = if (os.size <= 1) st"o" else st"(${(os, ", ")})"
          putss = putss(1 ~> (putss(1) :+
            methodSTF(T, info.name, info.ast.typeParams, Reflection.extractorName, T,
              ISZ[AST.Typed](AST.Typed.Name(info.name, for (_ <- info.ast.typeParams) yield AST.Typed.Name(ISZ(anyName), ISZ()))),
              (o: ST) =>
                if (os.isEmpty) st"if ($o) Some(T) else None()"
                else
                  st"""$o match {
                      |  case scala.Some($osST) => Some($osST)
                      |  case _ => None()
                      |}""")))
        }
      }
      for (v <- info.vars.values) {
        fields = fields :+ infoFieldST(F, params.get(v.ast.id.value).getOrElse(Reflection.Field.Kind.Normal), v.ast)
        putss = putss(0 ~> (putss(0) :+ methodST(F, info.name, info.ast.typeParams, v.ast.id.value, F, ISZ())))
        if (!v.ast.isVal) {
          putss = putss(1 ~> (putss(1) :+
            methodST(F, info.name, info.ast.typeParams, s"${v.ast.id.value}_=", T, ISZ(v.ast.tipeOpt.get.typedOpt.get))))
        }
      }
      for (m <- info.methods.values if m.ast.sig.params.size <= maxParams) {
        methods = methods :+ infoMethodST(F, info.name, m.ast.sig)
        putss = putss(m.ast.sig.params.size ~> (putss(m.ast.sig.params.size) :+
          methodST(F, info.name, info.ast.typeParams, m.ast.sig.id.value, m.ast.sig.hasParams, typesOfParams(m.ast.sig.params))))
      }
      val name = st"${(for (n <- info.name) yield ops.StringOps(n).escapeST, ".")}".render
      val nameValue = Reflection.objectOrTypeKey(name)
      val fieldsST: ST = if (fields.isEmpty) st"fields = ISZ()" else
        st"""fields = ISZ(
            |  ${(fields, ",\n")}
            |)"""
      val methodsST: ST = if (methods.isEmpty) st"methods = ISZ()" else
        st"""methods = ISZ(
            |  ${(methods, ",\n")}
            |)"""
      others = others :+
        st"""def info${infos.size} = Info( // ${(info.name, ".")}
            |  kind = Kind.$kind,
            |  $fieldsST,
            |  $methodsST
            |)"""
      infos = infos :+ st"""r.put(0x$nameValue, info${infos.size}) // objectOrTypeKey("$name").value"""
    }

    for (info <- nameMap.values) {
      info match {
        case info: Info.Object if !typeMap.contains(info.name) => genObject(info)
        case _ =>
      }
    }

    for (info <- typeMap.values) {
      info match {
        case info: TypeInfo.Sig => genSig(info)
        case info: TypeInfo.Adt => genAdt(info)
        case _ =>
      }
    }

    val lOpt: Option[ST] = licenseOpt match {
      case Some(l) => Some(
        st"""/*
            | ${(ops.StringOps(l).split((c: C) => c == '\n'), "\n")}
            |*/"""
      )
      case _ => None()
    }

    val r =
      st"""$lOpt
          |// Auto-generated
          |${packageNameOpt.map((p: String) => st"package $p")}
          |
          |import org.sireum._
          |import it.unimi.dsi.fastutil.ints.Int2ObjectOpenHashMap
          |import it.unimi.dsi.fastutil.longs.Long2ObjectOpenHashMap
          |
          |object $className {
          |  def create: Reflection = new $className
          |}
          |
          |import Reflection._
          |
          |class $className extends Reflection {
          |
          |  private lazy val nameMap: Int2ObjectOpenHashMap[Reflection.Info] = {
          |    val r = new Int2ObjectOpenHashMap[Info](${infos.size})
          |    ${(infos, "\n")}
          |    r
          |  }
          |
          |  ${(for (i <- 0 to maxParams if putss(i).nonEmpty) yield methodMapST(i, putss(i)), "\n\n")}
          |
          |  private def illegalReflection(title: String, isInObject: B, owner: String, name: String): Unit = {
          |    halt(s"$$title reflection $$owner$${if (isInObject) "." else "#"}$$name")
          |  }
          |
          |  override def info(name: String): Option[Info] = {
          |    val r = nameMap.get(objectOrTypeKey(name).value)
          |    if (r == null) None() else Some(r)
          |  }
          |
          |  ${(for (i <- 0 to maxParams if putss(i).nonEmpty) yield invokeST(i), "\n\n")}
          |
          |  ${(others, "\n\n")}
          |
          |  @inline def X[T](o: $anyName): T = o.asInstanceOf[T]
          |
          |  @inline def X[T](o: Option[_]): T = o.get.asInstanceOf[T]
          |
          |  override def string: String = "$className"
          |}"""
    outputOpt match {
      case Some(output) =>
        val f = output /+ packageNameOpt.map((s: String) => ops.StringOps(s).split((c: C) => c == '.')).getOrElse(ISZ()) / s"$className.scala"
        f.up.mkdirAll()
        f.writeOver(r.render)
        println(s"Wrote $f")
      case _ =>
        println(r.render)
    }
  }
}

/*
 Copyright (c) 2021, Robby, Kansas State University
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

import org.objectweb.asm.{
  AnnotationVisitor, Attribute, ClassReader, ClassVisitor, ClassWriter, MethodVisitor, Opcodes, Type, TypePath
}

import java.nio.file.{FileSystems, Files, Path, Paths}

object Proyek_Ext {

  val api: Int = Opcodes.ASM9

  class MVisitor(visitor: MethodVisitor) extends MethodVisitor(api) {

    override def visitParameter(name: Predef.String, access: Int): Unit =
      visitor.visitParameter(name, access)

    override def visitAnnotationDefault(): AnnotationVisitor =
      visitor.visitAnnotationDefault()

    override def visitAnnotation(descriptor: Predef.String, visible: Boolean): AnnotationVisitor =
      visitor.visitAnnotation(descriptor, visible)

    override def visitAnnotableParameterCount(parameterCount: Int, visible: Boolean): Unit =
      visitor.visitAnnotableParameterCount(parameterCount, visible)

    override def visitParameterAnnotation(parameter: Int, descriptor: Predef.String, visible: Boolean): AnnotationVisitor =
      visitor.visitParameterAnnotation(parameter, descriptor, visible)

    override def visitTypeAnnotation(typeRef: Int, typePath: TypePath, descriptor: Predef.String, visible: Boolean): AnnotationVisitor =
      visitor.visitTypeAnnotation(typeRef, typePath, descriptor, visible)

    override def visitAttribute(attribute: Attribute): Unit =
      visitor.visitAttribute(attribute)

    override def visitCode(): Unit = {
      visitor.visitCode()
      visitor.visitMethodInsn(Opcodes.INVOKESTATIC, Type.getInternalName(classOf[org.sireum.$internal.UnsafeUtils]),
        "releaseFence", Type.getMethodDescriptor(Type.VOID_TYPE), false)
      visitor.visitInsn(Opcodes.RETURN)
    }

    override def visitEnd(): Unit = {
      visitor.visitEnd()
    }
  }

  class CVisitor(cw: ClassWriter) extends ClassVisitor(api, cw) {
    override def visitMethod(access: Int, name: Predef.String, descriptor: Predef.String, signature: Predef.String,
                             exceptions: Array[Predef.String]): MethodVisitor = {
      val visitor = super.visitMethod(access, name, descriptor, signature, exceptions)
      return if (name == "releaseFence") new MVisitor(visitor) else visitor
    }
  }

  def compile(mid: String,
              category: String,
              javaHome: Os.Path,
              scalaHome: Os.Path,
              scalacOptions: ISZ[String],
              javacOptions: ISZ[String],
              classpath: ISZ[Os.Path],
              sourceFiles: ISZ[Os.Path],
              outDir: Os.Path): (B, String) = {

    if (sourceFiles.isEmpty) {
      return (T, "")
    }

    var scalaArgs = ISZ[String]("-classpath", st"${(classpath, Os.pathSep)}".render)
    scalaArgs = scalaArgs :+ "-d" :+ outDir.string
    var javaArgs = scalaArgs

    var ok = T
    val sb = new StringBuilder

    val javaSources: ISZ[String] = for (f <- sourceFiles if f.ext === "java") yield f.string
    val numOfScalaFiles: Z = sourceFiles.size - javaSources.size
    val numOfJavaFiles: Z = javaSources.size

    (numOfScalaFiles, numOfJavaFiles) match {
      case (z"0", _) => sb.append(s"* Compiled $numOfJavaFiles Java $mid $category source file${if (numOfJavaFiles > 1) "s" else ""}\n")
      case (_, z"0") => sb.append(s"* Compiled $numOfScalaFiles Scala $mid $category source file${if (numOfScalaFiles > 1) "s" else ""}\n")
      case (_, _) => sb.append(s"* Compiled $numOfScalaFiles Scala and $numOfJavaFiles Java $mid $category source files\n")
    }

    if (numOfScalaFiles > 0) {
      val scalac: Os.Path = if (Os.isWin) scalaHome / "bin" / "scalac.bat" else scalaHome / "bin" / "scalac"
      scalaArgs = scalaArgs ++ scalacOptions
      scalaArgs = scalaArgs ++ (for (f <- sourceFiles) yield f.string)

      val argFile = outDir.up / "scalac-args"
      argFile.writeOver(st"${(scalaArgs, "\n")}".render)
      val r = proc"$scalac @${argFile.name}".at(argFile.up.canon).run()
      ok = r.ok
      sb.append(r.out.value)
      sb.append(r.err.value)
    }

    if (ok) {
      if (javaSources.nonEmpty) {
        javaArgs = javaArgs ++ javacOptions
        javaArgs = javaArgs ++ javaSources
        val argFile = outDir.up / "javac-args"
        argFile.writeOver(st"${(javaArgs, "\n")}".render)
        val javac: Os.Path = if (Os.isWin) javaHome / "bin" / "javac.bat" else javaHome / "bin" / "javac"
        val r = proc"$javac @${argFile.name}".at(argFile.up.canon).console.run()
        sb.append(r.out.value)
        sb.append(r.err.value)
        return (r.ok, sb.toString)
      } else {
        return (T, sb.toString)
      }
    } else {
      return (F, sb.toString)
    }
  }

  def rewriteReleaseFence(jar: Os.Path): Unit = {
    val fs = FileSystems.newFileSystem(Paths.get(jar.value.value), null.asInstanceOf[ClassLoader])

    def process(p: Path): Unit = {
      if (!Files.exists(p)) {
        return
      }
      val is = Files.newInputStream(p)
      val cr = new ClassReader(is)
      val cw = new ClassWriter(ClassWriter.COMPUTE_MAXS | ClassWriter.COMPUTE_FRAMES)
      val cv = new CVisitor(cw)
      cr.accept(cv, ClassReader.SKIP_FRAMES)
      is.close()
      Files.write(p, cw.toByteArray)
    }

    process(fs.getPath("/scala/collection/immutable/VM.class"))
    process(fs.getPath("/scala/runtime/Statics.class"))
    fs.close()
  }

  def test(args: ISZ[String]): Unit = org.scalatest.tools.Runner.main(args.elements.map(_.value).toArray)

}

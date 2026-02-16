/*
 Copyright (c) 2017-2026,Robby, Kansas State University
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

object Compile_Ext {

  def javac(javacFile: String, argFile: String, env: ISZ[(String, String)]): (Z, String, String) = {
    if ($internal.Macro.isNative) {
      val r = Os.proc(ISZ(javacFile, s"@$argFile")).env(env).run()
      return (r.exitCode, r.out, r.err)
    } else {
      val compiler = javax.tools.ToolProvider.getSystemJavaCompiler
      if (compiler == null) {
        return (-1, "", "Error: no system Java compiler available")
      }
      val outBaos = new java.io.ByteArrayOutputStream()
      val errBaos = new java.io.ByteArrayOutputStream()
      val rc = compiler.run(null, outBaos, errBaos, s"@${argFile.value}")
      return (rc, String(outBaos.toString), String(errBaos.toString))
    }
  }

  def scalac(scalacFile: String, argFile: String, outDir: String, env: ISZ[(String, String)]): (Z, String, String) = {
    val useProcess = $internal.Macro.isNative || {
      val cp = System.getProperty("java.class.path")
      cp == null || !cp.contains("scala-compiler")
    }
    val result: (Z, String, String) = if (useProcess) {
      val r = (if (Os.isWin) Os.proc(ISZ("cmd", "/C", scalacFile, s"@$argFile"))
      else Os.proc(ISZ("bash", "-c", s"$scalacFile @$argFile"))).env(env).run()
      (r.exitCode, r.out, r.err)
    } else {
      val args = Array(s"@${argFile.value}")
      val outBaos = new java.io.ByteArrayOutputStream()
      val errBaos = new java.io.ByteArrayOutputStream()
      val outPs = new java.io.PrintStream(outBaos)
      // Set scala.home and scala.boot.class.path so the compiler finds scala-library.jar on Java 9+
      // (sun.boot.class.path is gone; the scalac script uses this custom property instead)
      if (System.getProperty("scala.boot.class.path") == null) {
        val scalaHomePath = new java.io.File(scalacFile.value).getParentFile.getParent
        System.setProperty("scala.home", scalaHomePath)
        val jars = new java.io.File(scalaHomePath, "lib").listFiles((_: java.io.File, n: Predef.String) => n.endsWith(".jar"))
        if (jars != null) {
          System.setProperty("scala.boot.class.path", jars.map(_.getAbsolutePath).mkString(java.io.File.pathSeparator))
        }
      }
      val errPs = new java.io.PrintStream(errBaos)
      try {
        val ok = scala.Console.withOut(outPs) {
          scala.Console.withErr(errPs) {
            val cls = Class.forName("scala.tools.nsc.MainClass")
            val main = cls.getConstructor().newInstance()
            val method = cls.getMethod("process", classOf[Array[Predef.String]])
            method.invoke(main, Array[Object](args): _*).asInstanceOf[java.lang.Boolean].booleanValue()
          }
        }
        outPs.flush()
        errPs.flush()
        val rc: Z = if (ok) 0 else 1
        (rc, String(outBaos.toString), String(errBaos.toString))
      } catch {
        case e: java.lang.reflect.InvocationTargetException =>
          outPs.flush()
          errPs.flush()
          val cause = if (e.getCause != null) e.getCause else e
          (-1, String(outBaos.toString), String(s"${errBaos.toString}Error invoking Scala compiler: ${cause.getMessage}"))
        case e: Exception =>
          outPs.flush()
          errPs.flush()
          (-1, String(outBaos.toString), String(s"${errBaos.toString}Error invoking Scala compiler: ${e.getMessage}"))
      }
    }
    // Run Z unboxing bytecode optimization on successful compilation
    if (result._1 == 0) {
      try {
        new org.sireum.lang.optimizer.ZUnboxer().transformDirectory(outDir.value)
      } catch {
        case t: Throwable =>
          t.printStackTrace()
      }
    }
    return result
  }

}

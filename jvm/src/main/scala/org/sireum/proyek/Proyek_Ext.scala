package org.sireum.proyek

import org.sireum._

object Proyek_Ext {

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
      val r = proc"$scalac @${argFile.name}".redirectErr.at(argFile.up.canon).run()
      ok = r.ok
      sb.append(r.out.value)
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
        return (r.ok, sb.toString)
      } else {
        return (T, sb.toString)
      }
    } else {
      return (F, sb.toString)
    }
  }

}

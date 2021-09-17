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

object Proyek {

  val metaInf: String = "META-INF"
  val manifestMf: String = "MANIFEST.MF"

  val mainOutDirName: String = "classes"
  val testOutDirName: String = "test-classes"
  val sourcesOutDirName: String = "sources"

  @strictpure def getProyekDir(path: Os.Path, outDirName: String, projectName: String, isJs: B): Os.Path =
    path / outDirName / s"$projectName${if (isJs) "-js" else ""}"

  @pure def normalizePath(path: String): String = {
    if (Os.isWin) {
      return path
    } else {
      return ops.StringOps(path).replaceAllChars('\\', '/')
    }
  }

  @strictpure def relUri(from: Os.Path, to: Os.Path): String = normalizePath(from.relativize(to).string)

  def loadVersions(path: Os.Path): Option[HashSMap[String, String]] = {
    if (!path.isFile) {
      return None()
    }
    val jsonParser = Json.Parser.create(path.read)
    val m = jsonParser.parseHashSMap(jsonParser.parseString _, jsonParser.parseString _)
    return if (jsonParser.errorOpt.isEmpty) Some(m) else None()
  }

  def storeVersions(path: Os.Path, versions: HashSMap[String, String]): Unit = {
    path.up.mkdirAll()
    path.writeOver(
      Json.Printer.printHashSMap(F, versions, Json.Printer.printString _, Json.Printer.printString _).render
    )
  }
}

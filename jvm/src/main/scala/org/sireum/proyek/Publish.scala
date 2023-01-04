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
import org.sireum.project._
import org.sireum.proyek.Proyek._

object Publish {

  val ignoredPathNames: HashSet[String] = HashSet ++ ISZ[String](
    ".git", ".DS_Store"
  )

  def run(path: Os.Path,
          outDirName: String,
          project: Project,
          projectName: String,
          dm: DependencyManager,
          isJs: B,
          orgName: ISZ[String],
          m2Repo: Os.Path,
          version: String,
          symlink: B): Z = {

    @strictpure def shouldCopy(p: Os.Path): B = !ignoredPathNames.contains(p.name)

    val m2Base = m2Repo /+ orgName
    m2Base.mkdirAll()

    val proyekDir = getProyekDir(path, outDirName, projectName, isJs)

    val projectOutDir = proyekDir / "modules"

    val target: Target.Type = if (isJs) Target.Js else Target.Jvm
    for (m <- project.modules.values if m.publishInfoOpt.nonEmpty && m.hasTarget(target)) {

      val mOutDir = projectOutDir / m.id

      val org = st"${(orgName, ".")}".render
      val hasScalaSource = Proyek.hasScalaSource(project)
      val module: String = if (hasScalaSource) s"${m.id}${if (isJs) dm.sjsSuffix else ""}_${dm.scalaMajorVersion}" else m.id

      val pom: String = {
        var deps = ISZ[ST]()

        for (mDep <- m.deps) {
          deps = deps :+ PomTemplate.dep(org,
            if (hasScalaSource) s"$mDep${if (isJs) dm.sjsSuffix else ""}_${dm.scalaMajorVersion}" else mDep,
            version)
        }

        for (lib <- dm.fetchDiffLibs(m)) {
          deps = deps :+ PomTemplate.dep(lib.org, lib.module, lib.version)
        }

        val pi = m.publishInfoOpt.get

        PomTemplate.pom(
          name = st"$org.$module".render,
          org = org,
          module = module,
          description = pi.description,
          version = version,
          url = pi.url,
          licenses = for (l <- pi.licenses) yield PomTemplate.license(l.name, l.url, l.distribution),
          developers = for (d <- pi.developers) yield PomTemplate.dev(d.id, d.name),
          dependencies = deps
        ).render

      }

      def writeMainJar(): Unit = {
        val mOutMainDir = mOutDir / mainOutDirName

        for (resource <- ProjectUtil.moduleResources(m)) {
          resource.overlayCopy(mOutMainDir, F, symlink, shouldCopy _, F)
        }

        val mainMetaInf = mOutMainDir / metaInf / manifestMf
        mainMetaInf.up.mkdirAll()
        mainMetaInf.writeOver(
          st"""Manifest-Version: 1.0
              |Created-By: Sireum Proyek
              |""".render
        )

        val mainPomXml = mOutMainDir / metaInf / org / module / "pom.xml"
        mainPomXml.writeOver(pom)

        val m2MainJar = m2Base / module / version / s"$module-$version.jar"
        m2MainJar.up.mkdirAll()
        mOutMainDir.zipTo(m2MainJar)
        println(s"Wrote $m2MainJar")
      }

      def writeSourcesJar(): Unit = {
        val mOutSourcesDir = mOutDir / sourcesOutDirName

        for (source <- ProjectUtil.moduleSources(m)) {
          source.overlayCopy(mOutSourcesDir, F, symlink, shouldCopy _, F)
        }

        for (resource <- ProjectUtil.moduleResources(m)) {
          resource.overlayCopy(mOutSourcesDir, F, symlink, shouldCopy _, F)
        }

        val sourcesMetaInf = mOutSourcesDir / metaInf / manifestMf
        sourcesMetaInf.up.mkdirAll()
        sourcesMetaInf.writeOver(
          st"""Manifest-Version: 1.0
              |Created-By: Sireum Proyek
              |""".render
        )

        val m2SourcesJar = m2Base / module / version / s"$module-$version-sources.jar"
        m2SourcesJar.up.mkdirAll()
        mOutSourcesDir.zipTo(m2SourcesJar)
        println(s"Wrote $m2SourcesJar")
      }

      def writePom(): Unit = {
        val m2Pom = m2Base / module / version / s"$module-$version.pom"
        m2Pom.up.mkdirAll()

        m2Pom.writeOver(pom)
        println(s"Wrote $m2Pom")
      }

      writeMainJar()
      writeSourcesJar()
      writePom()
    }

    return 0
  }

  object PomTemplate {

    @strictpure def license(name: String, url: String, distribution: String): ST =
      st"""<license>
          |  <name>$name</name>
          |  <url>$url</url>
          |  <distribution>$distribution</distribution>
          |</license>"""

    @strictpure def dev(id: String, name: String): ST =
      st"""<developer>
          |  <id>$id</id>
          |  <name>$name</name>
          |  <url>https://github.com/$id</url>
          |</developer>"""

    @strictpure def dep(org: String, module: String, version: String): ST =
      st"""<dependency>
          |  <groupId>$org</groupId>
          |  <artifactId>$module</artifactId>
          |  <version>$version</version>
          |</dependency>"""

    @pure def pom(name: String,
                  org: String,
                  module: String,
                  description: String,
                  version: String,
                  url: String,
                  licenses: ISZ[ST],
                  developers: ISZ[ST],
                  dependencies: ISZ[ST]): ST = {
      val urlOps = ops.StringOps(url)
      val i = urlOps.indexOf('/')
      val sshUrl = s"${urlOps.substring(0, i)}:${urlOps.substring(i + 1, url.size)}"
      val r =
        st"""<?xml version="1.0" encoding="UTF-8"?>
            |<project xsi:schemaLocation="http://maven.apache.org/POM/4.0.0 http://maven.apache.org/xsd/maven-4.0.0.xsd"
            |         xmlns="http://maven.apache.org/POM/4.0.0"
            |         xmlns:xsi="http://www.w3.org/2001/XMLSchema-instance">
            |  <modelVersion>4.0.0</modelVersion>
            |  <name>$name</name>
            |  <groupId>$org</groupId>
            |  <artifactId>$module</artifactId>
            |  <packaging>jar</packaging>
            |  <description>$description</description>
            |  <version>$version</version>
            |  <url>https://$url</url>
            |  <licenses>
            |    ${(licenses, "\n")}
            |  </licenses>
            |  <scm>
            |    <connection>scm:git:git://$url.git</connection>
            |    <developerConnection>scm:git:ssh://git@$sshUrl.git</developerConnection>
            |    <url>git://$url.git</url>
            |  </scm>
            |  <developers>
            |    ${(developers, "\n")}
            |  </developers>
            |  <dependencies>
            |    ${(dependencies, "\n")}
            |  </dependencies>
            |</project>
            |"""
      return r
    }
  }

}
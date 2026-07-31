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

import java.io.{FilterInputStream, InputStream}
import java.nio.ByteBuffer
import java.nio.channels.FileChannel
import java.nio.charset.StandardCharsets
import java.nio.file.attribute.{BasicFileAttributes, PosixFilePermissions}
import java.nio.file._
import java.util.EnumSet
import javax.xml.XMLConstants
import javax.xml.parsers.{DocumentBuilder, DocumentBuilderFactory}
import org.w3c.dom.{Document, Element, Node}
import org.xml.sax.{ErrorHandler, SAXException, SAXParseException}

import _root_.scala.collection.immutable.{Set => ScalaSet}
import _root_.scala.collection.mutable.ArrayBuffer
import _root_.scala.jdk.CollectionConverters._
import _root_.scala.util.control.NonFatal

object SagaReport_Ext {

  val InvalidTarget: Z = -7
  val XmlDirectoryError: Z = -8
  val EmissionError: Z = -9

  private[proyek] val ProducerId: Predef.String = "sireum-proyek-scalatest"
  private[proyek] val ProducerVersion: Predef.String = "1"
  private[proyek] val MaxXmlFiles: Int = 4096
  private[proyek] val MaxXmlBytes: Long = 16L * 1024L * 1024L
  private[proyek] val MaxTotalXmlBytes: Long = 64L * 1024L * 1024L

  def validateTarget(reportPath: String): (Z, String) = {
    try {
      checkedTarget(reportPath.value)
      return (0, "")
    } catch {
      case NonFatal(e) => return (InvalidTarget, String(errorText(e)))
    }
  }

  def createXmlDirectory(baseDir: String): (Z, String) = {
    try {
      val base = Paths.get(baseDir.value).toAbsolutePath.normalize
      Files.createDirectories(base)
      if (Files.isSymbolicLink(base) || !Files.isDirectory(base, LinkOption.NOFOLLOW_LINKS)) {
        throw new ReportException(s"ScalaTest XML base is not a non-symlink directory: $base")
      }
      val dir =
        try {
          Files.createTempDirectory(
            base,
            ".saga-report-xml-",
            PosixFilePermissions.asFileAttribute(PosixFilePermissions.fromString("rwx------")))
        } catch {
          case _: UnsupportedOperationException => Files.createTempDirectory(base, ".saga-report-xml-")
        }
      return (0, String(dir.toAbsolutePath.normalize.toString))
    } catch {
      case NonFatal(e) => return (XmlDirectoryError, String(errorText(e)))
    }
  }

  def emitAndCleanup(
      xmlDir: String,
      reportPath: String,
      childExitCode: Z,
      requestedSuiteNames: ISZ[String]): (Z, String) = {
    val dir = Paths.get(xmlDir.value).toAbsolutePath.normalize
    try {
      val parsed = parseDirectory(dir)
      requireRequestedSuites(requestedSuiteNames, parsed.suiteNames)
      requireCompletedSession(childExitCode, parsed.hasFailedOrError)
      publishBytes(checkedTarget(reportPath.value), SagaReportWire.renderBytes(parsed.report))
      return (0, "")
    } catch {
      case NonFatal(e) => return (EmissionError, String(errorText(e)))
    } finally {
      deleteTreeNoFollow(dir)
    }
  }

  private def requireCompletedSession(
      childExitCode: Z,
      hasFailedOrError: Boolean): Unit = {
    if (childExitCode == 0) {
      return
    }
    if (childExitCode == 1 && hasFailedOrError) {
      return
    }
    if (childExitCode == 1) {
      throw new ReportException(
        "ScalaTest exit code 1 has no Failed or Error XML outcome; " +
          "session completion cannot be established, refusing saga report publication")
    }
    throw new ReportException(
      s"ScalaTest exit code $childExitCode is not a completed Runner exit (expected 0 or 1); " +
        "refusing saga report publication")
  }

  private def requireRequestedSuites(
      requestedSuiteNames: ISZ[String],
      producedSuiteNames: _root_.scala.collection.Set[Predef.String]): Unit = {
    requestedSuiteNames.elements.foreach { requested =>
      val name = requested.value.trim
      if (!producedSuiteNames.contains(name)) {
        throw new ReportException(
          s"ScalaTest produced no testsuite for requested suite '$name'; " +
            "session completion cannot be established, refusing saga report publication")
      }
    }
  }

  private[proyek] def emitDirectory(xmlDir: Path, reportPath: Path): Unit = {
    val parsed = parseDirectory(xmlDir.toAbsolutePath.normalize)
    publishBytes(checkedTarget(reportPath.toString), SagaReportWire.renderBytes(parsed.report))
  }

  private[proyek] def publishForTest(
      bytes: Array[Byte],
      reportPath: Path,
      beforeLink: Path => Unit = _ => ()): Unit = {
    publishBytes(checkedTarget(reportPath.toString), bytes, beforeLink)
  }

  private[proyek] def validateTargetForTest(reportPath: Path): Unit = {
    checkedTarget(reportPath.toString)
    ()
  }

  private def errorText(e: Throwable): Predef.String = {
    val message = e.getMessage
    if (message == null || message.isEmpty) e.getClass.getSimpleName else message
  }

  private def checkedTarget(raw: Predef.String): Path = {
    if (raw == null || raw.isEmpty) {
      throw new ReportException("Saga report path must be nonempty")
    }
    val input =
      try Paths.get(raw)
      catch {
        case e: InvalidPathException => throw new ReportException(s"Invalid saga report path: ${e.getMessage}")
      }
    input.iterator.asScala.foreach { segment =>
      if (segment.toString == "..") {
        throw new ReportException(s"Saga report path must not contain traversal: $raw")
      }
    }
    val target = input.toAbsolutePath.normalize
    if (target.getFileName == null) {
      throw new ReportException(s"Saga report path must name a file: $target")
    }
    if (Files.exists(target, LinkOption.NOFOLLOW_LINKS)) {
      throw new ReportException(s"Saga report path already exists: $target")
    }
    val parent = target.getParent
    if (parent == null || !Files.exists(parent, LinkOption.NOFOLLOW_LINKS)) {
      throw new ReportException(s"Saga report parent must already exist: $parent")
    }
    rejectSymlinkComponents(parent)
    if (!Files.isDirectory(parent, LinkOption.NOFOLLOW_LINKS)) {
      throw new ReportException(s"Saga report parent is not a directory: $parent")
    }
    target
  }

  private def rejectSymlinkComponents(path: Path): Unit = {
    val absolute = path.toAbsolutePath.normalize
    var current = absolute.getRoot
    absolute.iterator.asScala.foreach { segment =>
      current = if (current == null) segment else current.resolve(segment)
      if (Files.isSymbolicLink(current)) {
        throw new ReportException(s"Saga report path contains a symbolic link: $current")
      }
      if (!Files.exists(current, LinkOption.NOFOLLOW_LINKS)) {
        throw new ReportException(s"Saga report path component does not exist: $current")
      }
    }
  }

  private def publishBytes(
      target: Path,
      bytes: Array[Byte],
      beforeLink: Path => Unit = _ => ()): Unit = {
    val parent = target.getParent
    var temp: Path = null
    try {
      temp = Files.createTempFile(parent, s".${target.getFileName}.", ".tmp")
      val channel = FileChannel.open(temp, StandardOpenOption.WRITE, StandardOpenOption.TRUNCATE_EXISTING)
      try {
        val buffer = ByteBuffer.wrap(bytes)
        while (buffer.hasRemaining) {
          channel.write(buffer)
        }
        channel.force(true)
      } finally {
        channel.close()
      }
      beforeLink(temp)
      if (Files.exists(target, LinkOption.NOFOLLOW_LINKS)) {
        throw new ReportException(s"Saga report path appeared before publication: $target")
      }
      // Creating the final hard link is an atomic, no-replace publication of
      // the already complete and fsynced inode.
      Files.createLink(target, temp)
    } finally {
      if (temp != null) {
        try Files.deleteIfExists(temp)
        catch {
          case NonFatal(_) =>
        }
      }
    }
  }

  private final case class ParsedDirectory(
      report: SagaReportWire.Report,
      suiteNames: _root_.scala.collection.Set[Predef.String],
      hasFailedOrError: Boolean)

  private final case class ParsedSuite(
      name: Predef.String,
      results: Vector[SagaReportWire.Result],
      obligationCount: Long,
      hasFailedOrError: Boolean)

  private final class DiagnosticCollector {
    private val diagnostics = ArrayBuffer.empty[SagaReportWire.Diagnostic]
    private val seen =
      _root_.scala.collection.mutable.Set.empty[(Predef.String, Predef.String)]
    private var overflow = false

    def addError(code: Predef.String, message: Predef.String): Unit = {
      val normalized = boundedUtf8Prefix(message, SagaReportWire.MaxMessageUtf8Bytes)._1
      if (seen.add((code, normalized))) {
        if (diagnostics.size < SagaReportWire.MaxDiagnostics - 1) {
          diagnostics += SagaReportWire.Diagnostic(
            SagaReportWire.Severity.Error,
            code,
            normalized)
        } else {
          overflow = true
        }
      }
    }

    def result(): Vector[SagaReportWire.Diagnostic] = {
      if (overflow) {
        diagnostics += SagaReportWire.Diagnostic(
          SagaReportWire.Severity.Error,
          "crucible.diagnostics.overflow",
          "additional ScalaTest diagnostics exceeded the report bound")
      }
      diagnostics.sortWith { (left, right) =>
        val code = SagaReportWire.compareText(left.code, right.code)
        code < 0 || code == 0 &&
          SagaReportWire.compareText(left.message, right.message) < 0
      }.toVector
    }
  }

  private def parseDirectory(dir: Path): ParsedDirectory = {
    if (Files.isSymbolicLink(dir) || !Files.isDirectory(dir, LinkOption.NOFOLLOW_LINKS)) {
      throw new ReportException(s"ScalaTest XML path is not a non-symlink directory: $dir")
    }
    val realDir = dir.toRealPath(LinkOption.NOFOLLOW_LINKS)
    val entries = ArrayBuffer.empty[Path]
    val stream = Files.newDirectoryStream(realDir)
    try {
      stream.iterator.asScala.foreach(entries += _)
    } finally {
      stream.close()
    }
    if (entries.isEmpty) {
      throw new ReportException("ScalaTest produced no JUnit XML documents")
    }
    if (entries.size > MaxXmlFiles) {
      throw new ReportException(s"ScalaTest XML document count exceeds $MaxXmlFiles")
    }
    entries.foreach { entry =>
      val name = entry.getFileName.toString
      if (!name.endsWith(".xml") || name == "." || name == "..") {
        throw new ReportException(s"Unexpected entry in ScalaTest XML directory: $name")
      }
      if (Files.isSymbolicLink(entry)) {
        throw new ReportException(s"ScalaTest XML document is a symbolic link: $entry")
      }
      if (entry.toAbsolutePath.normalize.getParent != realDir) {
        throw new ReportException(s"ScalaTest XML document traverses its private directory: $entry")
      }
      val attrs = Files.readAttributes(entry, classOf[BasicFileAttributes], LinkOption.NOFOLLOW_LINKS)
      if (!attrs.isRegularFile) {
        throw new ReportException(s"ScalaTest XML entry is not a regular file: $entry")
      }
      if (attrs.size <= 0 || attrs.size > MaxXmlBytes) {
        throw new ReportException(s"ScalaTest XML document has invalid size ${attrs.size}: $entry")
      }
    }
    val sorted = entries.sortWith((left, right) =>
      SagaReportWire.compareText(left.getFileName.toString, right.getFileName.toString) < 0)
    var totalBytes = 0L
    val suiteNames = _root_.scala.collection.mutable.Set.empty[Predef.String]
    val results = ArrayBuffer.empty[SagaReportWire.Result]
    val diagnostics = new DiagnosticCollector
    var obligationCount = 0L
    var hasFailedOrError = false
    sorted.foreach { entry =>
      val before = Files.readAttributes(entry, classOf[BasicFileAttributes], LinkOption.NOFOLLOW_LINKS)
      totalBytes = Math.addExact(totalBytes, before.size)
      if (totalBytes > MaxTotalXmlBytes) {
        throw new ReportException(s"ScalaTest XML input exceeds $MaxTotalXmlBytes total bytes")
      }
      val parsed = parseDocument(
        entry,
        before.size,
        SagaReportWire.MaxSelectedObligations - results.size,
        diagnostics)
      if (!suiteNames.add(parsed.name)) {
        throw new ReportException(s"Duplicate ScalaTest suite: ${parsed.name}")
      }
      results ++= parsed.results
      obligationCount = Math.addExact(obligationCount, parsed.obligationCount)
      hasFailedOrError = hasFailedOrError || parsed.hasFailedOrError
      val after = Files.readAttributes(entry, classOf[BasicFileAttributes], LinkOption.NOFOLLOW_LINKS)
      if (!after.isRegularFile || after.size != before.size ||
          (before.fileKey != null && after.fileKey != null && before.fileKey != after.fileKey)) {
        throw new ReportException(s"ScalaTest XML document changed while it was parsed: $entry")
      }
    }
    if (obligationCount > SagaReportWire.MaxSelectedObligations) {
      diagnostics.addError(
        "crucible.session.count.overflow",
        "ScalaTest testcase count exceeds the GateReport bound; excess results were omitted")
    }
    val ordered = results.sortWith((left, right) =>
      SagaReportWire.obligationCompare(left.obligation, right.obligation) < 0).toVector
    var i = 1
    while (i < ordered.size) {
      if (SagaReportWire.obligationCompare(ordered(i - 1).obligation, ordered(i).obligation) == 0) {
        val duplicate = ordered(i).obligation
        throw new ReportException(
          s"Duplicate ScalaTest obligation: ${duplicate.namespace} / ${duplicate.id}")
      }
      i += 1
    }
    val obligations = ordered.map(_.obligation)
    val report = SagaReportWire.Report(
      producerId = ProducerId,
      producerVersion = ProducerVersion,
      selectedObligations = obligations,
      results = ordered,
      metrics = Vector.empty,
      diagnostics = diagnostics.result())
    SagaReportWire.validate(report)
    ParsedDirectory(report, suiteNames.toSet, hasFailedOrError)
  }

  private def parseDocument(
      path: Path,
      size: Long,
      capacity: Int,
      diagnostics: DiagnosticCollector): ParsedSuite = {
    val builder = secureDocumentBuilder()
    val raw = Files.newInputStream(path, StandardOpenOption.READ)
    val bounded = new BoundedInputStream(raw, Math.min(size, MaxXmlBytes))
    val document =
      try builder.parse(bounded)
      finally bounded.close()
    parseSuite(document, capacity, diagnostics)
  }

  private def secureDocumentBuilder(): DocumentBuilder = {
    val factory = DocumentBuilderFactory.newInstance()
    factory.setNamespaceAware(true)
    factory.setXIncludeAware(false)
    factory.setExpandEntityReferences(false)
    factory.setFeature(XMLConstants.FEATURE_SECURE_PROCESSING, true)
    factory.setFeature("http://apache.org/xml/features/disallow-doctype-decl", true)
    factory.setFeature("http://xml.org/sax/features/external-general-entities", false)
    factory.setFeature("http://xml.org/sax/features/external-parameter-entities", false)
    factory.setFeature("http://apache.org/xml/features/nonvalidating/load-external-dtd", false)
    factory.setAttribute(XMLConstants.ACCESS_EXTERNAL_DTD, "")
    factory.setAttribute(XMLConstants.ACCESS_EXTERNAL_SCHEMA, "")
    val builder = factory.newDocumentBuilder()
    builder.setEntityResolver((publicId: Predef.String, systemId: Predef.String) => {
      throw new SAXException(s"External XML entity resolution is disabled: $publicId / $systemId")
    })
    builder.setErrorHandler(new ErrorHandler {
      override def warning(exception: SAXParseException): Unit = throw exception
      override def error(exception: SAXParseException): Unit = throw exception
      override def fatalError(exception: SAXParseException): Unit = throw exception
    })
    builder
  }

  private def parseSuite(
      document: Document,
      capacity: Int,
      diagnostics: DiagnosticCollector): ParsedSuite = {
    val root = document.getDocumentElement
    if (root == null || root.getTagName != "testsuite" ||
        _root_.scala.Option(root.getNamespaceURI).exists(_.nonEmpty)) {
      throw new ReportException("JUnit XML root must be an unqualified testsuite")
    }
    requireAttributes(
      root,
      required = ScalaSet("name", "tests", "failures", "errors", "time"),
      optional = ScalaSet("timestamp", "hostname", "skipped", "disabled"))
    val suiteName = requiredIdentifier(root, "name")
    val declaredTests = nonnegativeCount(root, "tests")
    val declaredFailures = nonnegativeCount(root, "failures")
    val declaredErrors = nonnegativeCount(root, "errors")
    val declaredSkipped: _root_.scala.Option[BigInt] =
      if (root.hasAttribute("skipped"))
        _root_.scala.Some(nonnegativeCount(root, "skipped"))
      else _root_.scala.None
    val suiteDuration = durationMillis(
      requiredAttribute(root, "time"),
      s"suite '$suiteName'",
      diagnostics)

    val results = ArrayBuffer.empty[SagaReportWire.Result]
    var testcaseCount = 0L
    var failureCount = 0L
    var errorCount = 0L
    var skippedCount = 0L
    var hasFailedOrError = false
    var propertiesCount = 0
    var systemOutCount = 0
    var systemErrorCount = 0
    var systemError = ""
    children(root).foreach {
      case element: Element if element.getTagName == "testcase" =>
        testcaseCount = Math.addExact(testcaseCount, 1L)
        val parsed = parseTestcase(element, suiteName, diagnostics)
        parsed.outcome match {
          case SagaReportWire.Outcome.Failed =>
            failureCount += 1
            hasFailedOrError = true
          case SagaReportWire.Outcome.Error =>
            errorCount += 1
            hasFailedOrError = true
          case SagaReportWire.Outcome.Skipped => skippedCount += 1
          case _ =>
        }
        if (results.size < capacity) {
          results += parsed
        }
      case element: Element if element.getTagName == "properties" =>
        propertiesCount += 1
        if (propertiesCount > 1) {
          throw new ReportException("JUnit testsuite has duplicate properties elements")
        }
        validateProperties(element)
      case element: Element if element.getTagName == "system-out" =>
        systemOutCount += 1
        if (systemOutCount > 1) {
          throw new ReportException("JUnit testsuite has duplicate system-out elements")
        }
        requireAttributes(element, ScalaSet.empty, ScalaSet.empty)
        requireTextOnly(element)
      case element: Element if element.getTagName == "system-err" =>
        systemErrorCount += 1
        if (systemErrorCount > 1) {
          throw new ReportException("JUnit testsuite has duplicate system-err elements")
        }
        requireAttributes(element, ScalaSet.empty, ScalaSet.empty)
        systemError = normalizeMessage(
          textOnly(element),
          s"suite '$suiteName'",
          diagnostics)
      case element: Element =>
        throw new ReportException(s"Unexpected JUnit testsuite element: ${element.getTagName}")
      case node if isIgnorable(node) =>
      case node =>
        throw new ReportException(s"Unexpected JUnit testsuite node type: ${node.getNodeType}")
    }

    if (propertiesCount != 1 || systemOutCount != 1 || systemErrorCount != 1) {
      throw new ReportException(
        "JUnit testsuite must contain exactly one properties, system-out, and system-err element")
    }
    if (declaredTests != BigInt(testcaseCount)) {
      throw new ReportException(
        s"JUnit tests count $declaredTests does not match $testcaseCount testcases")
    }
    if (declaredFailures != BigInt(failureCount)) {
      throw new ReportException(
        s"JUnit failures count $declaredFailures does not match $failureCount failure elements")
    }
    val suiteErrorCount = declaredErrors - BigInt(errorCount)
    if (suiteErrorCount < 0 || suiteErrorCount > 1) {
      throw new ReportException(
        s"JUnit errors count $declaredErrors is inconsistent with $errorCount testcase errors")
    }
    declaredSkipped.foreach { count =>
      if (count != BigInt(skippedCount)) {
        throw new ReportException(
          s"JUnit skipped count $count does not match $skippedCount skipped elements")
      }
    }
    if (suiteErrorCount == 1) {
      val message = if (systemError.nonEmpty) systemError else "ScalaTest suite aborted"
      hasFailedOrError = true
      if (results.size < capacity) {
        results += SagaReportWire.Result(
          obligation = SagaReportWire.Obligation(suiteName, "<suite-error>"),
          outcome = SagaReportWire.Outcome.Error,
          durationMillis = suiteDuration,
          metrics = Vector.empty,
          message = message)
      }
    }
    ParsedSuite(
      suiteName,
      results.toVector,
      Math.addExact(testcaseCount, suiteErrorCount.toLong),
      hasFailedOrError)
  }

  private def parseTestcase(
      element: Element,
      suiteName: Predef.String,
      diagnostics: DiagnosticCollector): SagaReportWire.Result = {
    requireAttributes(
      element,
      required = ScalaSet("name", "classname", "time"),
      optional = ScalaSet.empty)
    val name = requiredIdentifier(element, "name")
    val className = requiredAttribute(element, "classname")
    SagaReportWire.requireIdentifier(className, "testcase classname")
    if (className != suiteName) {
      throw new ReportException(
        s"JUnit testcase classname '$className' does not match suite '$suiteName'")
    }
    val subject = s"test '$suiteName::$name'"
    val outcomes = ArrayBuffer.empty[(SagaReportWire.Outcome, Predef.String)]
    children(element).foreach {
      case child: Element if child.getTagName == "failure" =>
        outcomes += ((
          SagaReportWire.Outcome.Failed,
          outcomeMessage(child, subject, diagnostics)))
      case child: Element if child.getTagName == "error" =>
        outcomes += ((
          SagaReportWire.Outcome.Error,
          outcomeMessage(child, subject, diagnostics)))
      case child: Element if child.getTagName == "skipped" =>
        outcomes += ((
          SagaReportWire.Outcome.Skipped,
          skippedMessage(child, subject, diagnostics)))
      case child: Element if child.getTagName == "system-out" || child.getTagName == "system-err" =>
        requireAttributes(child, ScalaSet.empty, ScalaSet.empty)
        requireTextOnly(child)
      case child: Element =>
        throw new ReportException(s"Unexpected JUnit testcase element: ${child.getTagName}")
      case node if isIgnorable(node) =>
      case node =>
        throw new ReportException(s"Unexpected JUnit testcase node type: ${node.getNodeType}")
    }
    if (outcomes.size > 1) {
      throw new ReportException(s"JUnit testcase has multiple terminal outcomes: $suiteName / $name")
    }
    val (outcome, message) =
      if (outcomes.isEmpty) (SagaReportWire.Outcome.Passed, "")
      else outcomes.head
    SagaReportWire.Result(
      obligation = SagaReportWire.Obligation(suiteName, name),
      outcome = outcome,
      durationMillis = durationMillis(
        requiredAttribute(element, "time"),
        subject,
        diagnostics),
      metrics = Vector.empty,
      message = message)
  }

  private def outcomeMessage(
      element: Element,
      subject: Predef.String,
      diagnostics: DiagnosticCollector): Predef.String = {
    requireAttributes(element, ScalaSet.empty, ScalaSet("message", "type"))
    val attribute = _root_.scala.Option(element.getAttribute("message")).getOrElse("")
    val body = textOnly(element).trim
    val message =
      if (attribute.nonEmpty && body.nonEmpty) s"$attribute\n$body"
      else if (attribute.nonEmpty) attribute
      else body
    normalizeMessage(message, subject, diagnostics)
  }

  private def skippedMessage(
      element: Element,
      subject: Predef.String,
      diagnostics: DiagnosticCollector): Predef.String = {
    requireAttributes(element, ScalaSet.empty, ScalaSet("message", "type"))
    val attribute = _root_.scala.Option(element.getAttribute("message")).getOrElse("")
    val body = textOnly(element).trim
    val message =
      if (attribute.nonEmpty && body.nonEmpty) s"$attribute\n$body"
      else if (attribute.nonEmpty) attribute
      else if (body.nonEmpty) body
      else "skipped"
    normalizeMessage(message, subject, diagnostics)
  }

  private def validateProperties(element: Element): Unit = {
    requireAttributes(element, ScalaSet.empty, ScalaSet.empty)
    val names = _root_.scala.collection.mutable.Set.empty[Predef.String]
    children(element).foreach {
      case property: Element if property.getTagName == "property" =>
        requireAttributes(property, ScalaSet("name", "value"), ScalaSet.empty)
        requireTextOnly(property)
        val name = requiredAttribute(property, "name")
        if (!names.add(name)) {
          throw new ReportException(s"Duplicate JUnit property name: $name")
        }
      case node if isIgnorable(node) =>
      case node =>
        throw new ReportException(s"Unexpected JUnit properties node type: ${node.getNodeType}")
    }
  }

  private def children(element: Element): Vector[Node] = {
    val result = Vector.newBuilder[Node]
    val nodes = element.getChildNodes
    var i = 0
    while (i < nodes.getLength) {
      result += nodes.item(i)
      i += 1
    }
    result.result()
  }

  private def isIgnorable(node: Node): Boolean = {
    (node.getNodeType == Node.TEXT_NODE || node.getNodeType == Node.CDATA_SECTION_NODE) &&
      _root_.scala.Option(node.getNodeValue).forall(_.trim.isEmpty) ||
      node.getNodeType == Node.COMMENT_NODE
  }

  private def textOnly(element: Element): Predef.String = {
    val result = new java.lang.StringBuilder
    children(element).foreach {
      case node if node.getNodeType == Node.TEXT_NODE || node.getNodeType == Node.CDATA_SECTION_NODE =>
        result.append(node.getNodeValue)
      case node if node.getNodeType == Node.COMMENT_NODE =>
      case node =>
        throw new ReportException(s"Element ${element.getTagName} must contain text only")
    }
    result.toString
  }

  private def requireTextOnly(element: Element): Unit = {
    textOnly(element)
    ()
  }

  private def requireAttributes(
      element: Element,
      required: ScalaSet[Predef.String],
      optional: ScalaSet[Predef.String]): Unit = {
    if (_root_.scala.Option(element.getNamespaceURI).exists(_.nonEmpty)) {
      throw new ReportException(s"JUnit element must be unqualified: ${element.getTagName}")
    }
    val seen = _root_.scala.collection.mutable.Set.empty[Predef.String]
    val attributes = element.getAttributes
    var i = 0
    while (i < attributes.getLength) {
      val attribute = attributes.item(i)
      val name = attribute.getNodeName
      if (_root_.scala.Option(attribute.getNamespaceURI).exists(_.nonEmpty) ||
          (!required.contains(name) && !optional.contains(name))) {
        throw new ReportException(s"Unexpected attribute '$name' on ${element.getTagName}")
      }
      if (!seen.add(name)) {
        throw new ReportException(s"Duplicate attribute '$name' on ${element.getTagName}")
      }
      i += 1
    }
    required.foreach { name =>
      if (!seen.contains(name)) {
        throw new ReportException(s"Missing attribute '$name' on ${element.getTagName}")
      }
    }
  }

  private def requiredAttribute(element: Element, name: Predef.String): Predef.String = {
    if (!element.hasAttribute(name)) {
      throw new ReportException(s"Missing attribute '$name' on ${element.getTagName}")
    }
    element.getAttribute(name)
  }

  private def requiredIdentifier(element: Element, name: Predef.String): Predef.String = {
    val value = requiredAttribute(element, name)
    SagaReportWire.requireIdentifier(value, s"${element.getTagName} $name")
    value
  }

  private def nonnegativeCount(element: Element, name: Predef.String): BigInt = {
    val text = requiredAttribute(element, name)
    if (!text.matches("0|[1-9][0-9]*")) {
      throw new ReportException(s"Invalid nonnegative JUnit count '$text' for $name")
    }
    try BigInt(text)
    catch {
      case _: NumberFormatException =>
        throw new ReportException(s"Invalid JUnit count '$text' for $name")
    }
  }

  private def durationMillis(
      text: Predef.String,
      subject: Predef.String,
      diagnostics: DiagnosticCollector): Long = {
    if (!text.matches("-?(0|[1-9][0-9]*)(\\.[0-9]+)?")) {
      throw new ReportException(s"Invalid JUnit duration '$text'")
    }
    val negative = text.charAt(0) == '-'
    val unsigned = if (negative) text.substring(1) else text
    val dot = unsigned.indexOf('.')
    val seconds = if (dot < 0) unsigned else unsigned.substring(0, dot)
    val fraction = if (dot < 0) "" else unsigned.substring(dot + 1)
    if (fraction.length > 3 && !fraction.substring(3).forall(_ == '0')) {
      throw new ReportException(s"JUnit duration is not a whole millisecond: $text")
    }
    val fractionMillis = {
      val padded = fraction + "000"
      padded.substring(0, 3).toInt
    }
    val nonzero = seconds.exists(_ != '0') || fractionMillis != 0
    if (negative && nonzero) {
      diagnostics.addError(
        "crucible.test.duration.invalid",
        s"$subject reported a negative duration")
      return 0L
    }
    if (seconds.length > 6) {
      diagnostics.addError(
        "crucible.test.duration.invalid",
        s"$subject exceeded the duration bound")
      return SagaReportWire.MaxDurationMillis
    }
    val millis =
      try Math.addExact(Math.multiplyExact(seconds.toLong, 1000L), fractionMillis.toLong)
      catch {
        case _: ArithmeticException =>
          diagnostics.addError(
            "crucible.test.duration.invalid",
            s"$subject exceeded the duration bound")
          return SagaReportWire.MaxDurationMillis
      }
    if (millis > SagaReportWire.MaxDurationMillis) {
      diagnostics.addError(
        "crucible.test.duration.invalid",
        s"$subject exceeded the duration bound")
      SagaReportWire.MaxDurationMillis
    } else {
      millis
    }
  }

  private def normalizeMessage(
      value: Predef.String,
      subject: Predef.String,
      diagnostics: DiagnosticCollector): Predef.String = {
    val normalized = boundedUtf8Prefix(value, SagaReportWire.MaxMessageUtf8Bytes)
    if (normalized._2) {
      diagnostics.addError(
        "crucible.test.message.invalid",
        s"$subject emitted an invalid or oversized message")
    }
    normalized._1
  }

  private def boundedUtf8Prefix(
      value: Predef.String,
      maximum: Int): (Predef.String, Boolean) = {
    if (value == null) {
      return ("", true)
    }
    val result = new java.lang.StringBuilder
    var bytes = 0
    var changed = false
    var i = 0
    while (i < value.length && bytes < maximum) {
      val current = value.charAt(i)
      val (codePoint, width) =
        if (Character.isHighSurrogate(current)) {
          if (i + 1 < value.length && Character.isLowSurrogate(value.charAt(i + 1))) {
            (Character.toCodePoint(current, value.charAt(i + 1)), 2)
          } else {
            changed = true
            (0xFFFD, 1)
          }
        } else if (Character.isLowSurrogate(current)) {
          changed = true
          (0xFFFD, 1)
        } else {
          (current.toInt, 1)
        }
      val encoded =
        if (codePoint <= 0x7F) 1
        else if (codePoint <= 0x7FF) 2
        else if (codePoint <= 0xFFFF) 3
        else 4
      if (bytes + encoded > maximum) {
        changed = true
        i = value.length
      } else {
        result.appendCodePoint(codePoint)
        bytes += encoded
        i += width
      }
    }
    if (i < value.length) {
      changed = true
    }
    (result.toString, changed)
  }

  private def deleteTreeNoFollow(path: Path): Unit = {
    try {
      if (!Files.exists(path, LinkOption.NOFOLLOW_LINKS)) {
        return
      }
      Files.walkFileTree(
        path,
        EnumSet.noneOf(classOf[FileVisitOption]),
        Int.MaxValue,
        new SimpleFileVisitor[Path] {
          override def visitFile(file: Path, attrs: BasicFileAttributes): FileVisitResult = {
            Files.deleteIfExists(file)
            FileVisitResult.CONTINUE
          }

          override def postVisitDirectory(dir: Path, exc: java.io.IOException): FileVisitResult = {
            Files.deleteIfExists(dir)
            FileVisitResult.CONTINUE
          }
        })
    } catch {
      case NonFatal(_) =>
    }
  }

  private final class BoundedInputStream(in: InputStream, maximum: Long)
      extends FilterInputStream(in) {
    private var count = 0L

    private def add(amount: Int): Unit = {
      if (amount > 0) {
        count = Math.addExact(count, amount.toLong)
        if (count > maximum) {
          throw new ReportException(s"XML document grew beyond its bounded size of $maximum bytes")
        }
      }
    }

    override def read(): Int = {
      val result = super.read()
      if (result >= 0) add(1)
      result
    }

    override def read(bytes: Array[Byte], offset: Int, length: Int): Int = {
      val result = super.read(bytes, offset, length)
      add(result)
      result
    }
  }

  private final class ReportException(message: Predef.String) extends RuntimeException(message)
}

private[proyek] object SagaReportWire {

  val MaxIdentifierUtf8Bytes: Int = 1024
  val MaxUnitUtf8Bytes: Int = 256
  val MaxMetricValueUtf8Bytes: Int = 256
  val MaxMessageUtf8Bytes: Int = 16384
  val MaxSelectedObligations: Int = 65536
  val MaxResultMetrics: Int = 256
  val MaxReportMetrics: Int = 4096
  val MaxTotalMetrics: Int = 65536
  val MaxDiagnostics: Int = 4096
  val MaxDurationMillis: Long = 604800000L

  sealed trait Outcome {
    def value: Predef.String
  }

  object Outcome {
    case object Passed extends Outcome {
      override val value: Predef.String = "Passed"
    }
    case object Failed extends Outcome {
      override val value: Predef.String = "Failed"
    }
    case object Skipped extends Outcome {
      override val value: Predef.String = "Skipped"
    }
    case object TimedOut extends Outcome {
      override val value: Predef.String = "TimedOut"
    }
    case object Error extends Outcome {
      override val value: Predef.String = "Error"
    }
  }

  sealed trait Severity {
    def value: Predef.String
    def rank: Int
  }

  object Severity {
    case object Info extends Severity {
      override val value: Predef.String = "Info"
      override val rank: Int = 0
    }
    case object Warning extends Severity {
      override val value: Predef.String = "Warning"
      override val rank: Int = 1
    }
    case object Error extends Severity {
      override val value: Predef.String = "Error"
      override val rank: Int = 2
    }
  }

  final case class Obligation(namespace: Predef.String, id: Predef.String)
  final case class Metric(id: Predef.String, unit: Predef.String, canonicalValue: Predef.String)
  final case class Result(
      obligation: Obligation,
      outcome: Outcome,
      durationMillis: Long,
      metrics: Vector[Metric],
      message: Predef.String)
  final case class Diagnostic(severity: Severity, code: Predef.String, message: Predef.String)
  final case class Report(
      producerId: Predef.String,
      producerVersion: Predef.String,
      selectedObligations: Vector[Obligation],
      results: Vector[Result],
      metrics: Vector[Metric],
      diagnostics: Vector[Diagnostic])

  def renderBytes(report: Report): Array[Byte] = {
    validate(report)
    val builder = new java.lang.StringBuilder
    appendReport(builder, report)
    builder.toString.getBytes(StandardCharsets.UTF_8)
  }

  def render(report: Report): Predef.String =
    new Predef.String(renderBytes(report), StandardCharsets.UTF_8)

  def isGreen(report: Report): Boolean = {
    try validate(report)
    catch {
      case _: IllegalArgumentException => return false
    }
    report.results.forall(_.outcome == Outcome.Passed) &&
      !report.diagnostics.exists(_.severity == Severity.Error)
  }

  def validate(report: Report): Unit = {
    requireIdentifier(report.producerId, "producerId")
    requireIdentifier(report.producerVersion, "producerVersion")
    if (report.selectedObligations.isEmpty ||
        report.selectedObligations.size > MaxSelectedObligations ||
        report.results.size != report.selectedObligations.size) {
      invalid("selected obligations and results are not bounded one-to-one nonempty sequences")
    }
    var totalMetrics = report.metrics.size.toLong
    var i = 0
    while (i < report.selectedObligations.size) {
      val obligation = report.selectedObligations(i)
      validateObligation(obligation)
      if (i > 0 && obligationCompare(report.selectedObligations(i - 1), obligation) >= 0) {
        invalid("selected obligations are not strictly ordered")
      }
      val result = report.results(i)
      if (result.obligation != obligation) {
        invalid("result obligation does not match selected obligation")
      }
      validateResult(result)
      totalMetrics += result.metrics.size
      if (totalMetrics > MaxTotalMetrics) {
        invalid(s"total metric count exceeds $MaxTotalMetrics")
      }
      i += 1
    }
    validateMetrics(report.metrics, MaxReportMetrics)
    if (report.diagnostics.size > MaxDiagnostics) {
      invalid(s"diagnostic count exceeds $MaxDiagnostics")
    }
    i = 0
    while (i < report.diagnostics.size) {
      val diagnostic = report.diagnostics(i)
      requireIdentifier(diagnostic.code, "diagnostic code")
      requireMessage(diagnostic.message, "diagnostic message")
      if (i > 0 && diagnosticCompare(report.diagnostics(i - 1), diagnostic) >= 0) {
        invalid("diagnostics are not strictly ordered")
      }
      i += 1
    }
  }

  def requireIdentifier(value: Predef.String, label: Predef.String): Unit = {
    requireBoundedText(value, 1, MaxIdentifierUtf8Bytes, label)
  }

  def requireMessage(value: Predef.String, label: Predef.String): Unit = {
    requireBoundedText(value, 0, MaxMessageUtf8Bytes, label)
  }

  def compareText(left: Predef.String, right: Predef.String): Int = {
    requireScalarText(left, "comparison text")
    requireScalarText(right, "comparison text")
    var li = 0
    var ri = 0
    while (li < left.length && ri < right.length) {
      val lc = Character.codePointAt(left, li)
      val rc = Character.codePointAt(right, ri)
      if (lc < rc) return -1
      if (lc > rc) return 1
      li += Character.charCount(lc)
      ri += Character.charCount(rc)
    }
    Integer.compare(left.length - li, right.length - ri)
  }

  def obligationCompare(left: Obligation, right: Obligation): Int = {
    val namespace = compareText(left.namespace, right.namespace)
    if (namespace != 0) namespace else compareText(left.id, right.id)
  }

  private def metricCompare(left: Metric, right: Metric): Int = {
    val id = compareText(left.id, right.id)
    if (id != 0) id else compareText(left.unit, right.unit)
  }

  private def diagnosticCompare(left: Diagnostic, right: Diagnostic): Int = {
    val severity = Integer.compare(left.severity.rank, right.severity.rank)
    if (severity != 0) return severity
    val code = compareText(left.code, right.code)
    if (code != 0) code else compareText(left.message, right.message)
  }

  private def validateObligation(obligation: Obligation): Unit = {
    requireIdentifier(obligation.namespace, "obligation namespace")
    requireIdentifier(obligation.id, "obligation id")
  }

  private def validateResult(result: Result): Unit = {
    validateObligation(result.obligation)
    if (result.durationMillis < 0 || result.durationMillis > MaxDurationMillis) {
      invalid(s"durationMillis is outside 0..$MaxDurationMillis")
    }
    validateMetrics(result.metrics, MaxResultMetrics)
    requireMessage(result.message, "result message")
  }

  private def validateMetrics(metrics: Vector[Metric], maximum: Int): Unit = {
    if (metrics.size > maximum) {
      invalid(s"metric count exceeds $maximum")
    }
    var i = 0
    while (i < metrics.size) {
      val metric = metrics(i)
      requireIdentifier(metric.id, "metric id")
      requireBoundedText(metric.unit, 1, MaxUnitUtf8Bytes, "metric unit")
      requireCanonicalDecimal(metric.canonicalValue)
      if (i > 0 && metricCompare(metrics(i - 1), metric) >= 0) {
        invalid("metrics are not strictly ordered")
      }
      i += 1
    }
  }

  private def requireCanonicalDecimal(value: Predef.String): Unit = {
    requireBoundedText(value, 1, MaxMetricValueUtf8Bytes, "metric canonical value")
    var i = 0
    if (value.charAt(0) == '0') {
      i = 1
      if (i < value.length && value.charAt(i) != '.') {
        invalid(s"metric value is not canonical decimal: $value")
      }
    } else {
      if (value.charAt(0) < '1' || value.charAt(0) > '9') {
        invalid(s"metric value is not canonical decimal: $value")
      }
      i = 1
      while (i < value.length && value.charAt(i) >= '0' && value.charAt(i) <= '9') {
        i += 1
      }
    }
    if (i == value.length) return
    if (value.charAt(i) != '.') {
      invalid(s"metric value is not canonical decimal: $value")
    }
    i += 1
    if (i == value.length) {
      invalid(s"metric value is not canonical decimal: $value")
    }
    while (i < value.length) {
      if (value.charAt(i) < '0' || value.charAt(i) > '9') {
        invalid(s"metric value is not canonical decimal: $value")
      }
      i += 1
    }
    if (value.charAt(value.length - 1) == '0') {
      invalid(s"metric value is not canonical decimal: $value")
    }
  }

  private def requireBoundedText(
      value: Predef.String,
      minimum: Int,
      maximum: Int,
      label: Predef.String): Unit = {
    requireScalarText(value, label)
    val size = value.getBytes(StandardCharsets.UTF_8).length
    if (size < minimum || size > maximum) {
      invalid(s"$label UTF-8 byte size $size is outside $minimum..$maximum")
    }
  }

  private def requireScalarText(value: Predef.String, label: Predef.String): Unit = {
    if (value == null) {
      invalid(s"$label is null")
    }
    var i = 0
    while (i < value.length) {
      val current = value.charAt(i)
      if (Character.isHighSurrogate(current)) {
        if (i + 1 >= value.length || !Character.isLowSurrogate(value.charAt(i + 1))) {
          invalid(s"$label contains an unpaired high surrogate")
        }
        i += 2
      } else if (Character.isLowSurrogate(current)) {
        invalid(s"$label contains an unpaired low surrogate")
      } else {
        i += 1
      }
    }
  }

  private def appendReport(builder: java.lang.StringBuilder, report: Report): Unit = {
    builder.append("""{"type":"org.sireum.gate.GateReport","producerId":""")
    appendQuoted(builder, report.producerId)
    builder.append(""","producerVersion":""")
    appendQuoted(builder, report.producerVersion)
    builder.append(""","selectedObligations":""")
    appendArray(builder, report.selectedObligations)(appendObligation)
    builder.append(""","results":""")
    appendArray(builder, report.results)(appendResult)
    builder.append(""","metrics":""")
    appendArray(builder, report.metrics)(appendMetric)
    builder.append(""","diagnostics":""")
    appendArray(builder, report.diagnostics)(appendDiagnostic)
    builder.append('}')
  }

  private def appendObligation(builder: java.lang.StringBuilder, obligation: Obligation): Unit = {
    builder.append("""{"type":"org.sireum.gate.GateObligation","namespace":""")
    appendQuoted(builder, obligation.namespace)
    builder.append(""","id":""")
    appendQuoted(builder, obligation.id)
    builder.append('}')
  }

  private def appendOutcome(builder: java.lang.StringBuilder, outcome: Outcome): Unit = {
    builder.append("""{"type":"org.sireum.gate.GateOutcome.Type","value":""")
    appendQuoted(builder, outcome.value)
    builder.append('}')
  }

  private def appendMetric(builder: java.lang.StringBuilder, metric: Metric): Unit = {
    builder.append("""{"type":"org.sireum.gate.GateMetric","id":""")
    appendQuoted(builder, metric.id)
    builder.append(""","unit":""")
    appendQuoted(builder, metric.unit)
    builder.append(""","canonicalValue":""")
    appendQuoted(builder, metric.canonicalValue)
    builder.append('}')
  }

  private def appendResult(builder: java.lang.StringBuilder, result: Result): Unit = {
    builder.append("""{"type":"org.sireum.gate.GateResult","obligation":""")
    appendObligation(builder, result.obligation)
    builder.append(""","outcome":""")
    appendOutcome(builder, result.outcome)
    builder.append(""","durationMillis":""")
    builder.append(result.durationMillis)
    builder.append(""","metrics":""")
    appendArray(builder, result.metrics)(appendMetric)
    builder.append(""","message":""")
    appendQuoted(builder, result.message)
    builder.append('}')
  }

  private def appendSeverity(builder: java.lang.StringBuilder, severity: Severity): Unit = {
    builder.append("""{"type":"org.sireum.gate.GateDiagnosticSeverity.Type","value":""")
    appendQuoted(builder, severity.value)
    builder.append('}')
  }

  private def appendDiagnostic(builder: java.lang.StringBuilder, diagnostic: Diagnostic): Unit = {
    builder.append("""{"type":"org.sireum.gate.GateDiagnostic","severity":""")
    appendSeverity(builder, diagnostic.severity)
    builder.append(""","code":""")
    appendQuoted(builder, diagnostic.code)
    builder.append(""","message":""")
    appendQuoted(builder, diagnostic.message)
    builder.append('}')
  }

  private def appendArray[A](
      builder: java.lang.StringBuilder,
      values: Vector[A])(
      append: (java.lang.StringBuilder, A) => Unit): Unit = {
    builder.append('[')
    var i = 0
    while (i < values.size) {
      if (i > 0) builder.append(',')
      append(builder, values(i))
      i += 1
    }
    builder.append(']')
  }

  private def appendQuoted(builder: java.lang.StringBuilder, value: Predef.String): Unit = {
    builder.append('"')
    var i = 0
    while (i < value.length) {
      val codePoint = Character.codePointAt(value, i)
      codePoint match {
        case 0x22 => builder.append("\\\"")
        case 0x5C => builder.append("\\\\")
        case 0x2F => builder.append("\\/")
        case 0x08 => builder.append("\\b")
        case 0x0C => builder.append("\\f")
        case 0x0A => builder.append("\\n")
        case 0x0D => builder.append("\\r")
        case 0x09 => builder.append("\\t")
        case cp if cp <= 0x1F || cp == 0x7F =>
          builder.append("\\u")
          val hex = Integer.toHexString(cp).toUpperCase(java.util.Locale.ROOT)
          var pad = hex.length
          while (pad < 4) {
            builder.append('0')
            pad += 1
          }
          builder.append(hex)
        case cp => builder.appendCodePoint(cp)
      }
      i += Character.charCount(codePoint)
    }
    builder.append('"')
  }

  private def invalid(message: Predef.String): Nothing =
    throw new IllegalArgumentException(message)
}

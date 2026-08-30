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
import org.sireum.test._

import java.nio.charset.StandardCharsets
import java.nio.file.{Files, LinkOption, Path, Paths}
import _root_.scala.jdk.CollectionConverters._

object SagaReportGolden {

  // Frozen wire vectors copied byte-for-byte from
  // V/compiler/jvm/src/test/scala/org/sireum/lang/GateReportJsonTreeTest.scala
  // lines 10 and 15.  They are fixture data only; this module never imports,
  // links, builds against, or resolves a V source/binary artifact.
  val GoldenTree: Predef.String =
    """{"type":"org.sireum.gate.GateReport","producerId":"producer\/\"\\edge","producerVersion":"v1-é😀","selectedObligations":[{"type":"org.sireum.gate.GateObligation","namespace":"gate.example","id":"01-passed"},{"type":"org.sireum.gate.GateObligation","namespace":"gate.example","id":"02-failed"},{"type":"org.sireum.gate.GateObligation","namespace":"gate.example","id":"03-skipped"},{"type":"org.sireum.gate.GateObligation","namespace":"gate.example","id":"04-timed-out"},{"type":"org.sireum.gate.GateObligation","namespace":"gate.example","id":"05-error"}],"results":[{"type":"org.sireum.gate.GateResult","obligation":{"type":"org.sireum.gate.GateObligation","namespace":"gate.example","id":"01-passed"},"outcome":{"type":"org.sireum.gate.GateOutcome.Type","value":"Passed"},"durationMillis":0,"metrics":[{"type":"org.sireum.gate.GateMetric","id":"attempts","unit":"count","canonicalValue":"0"},{"type":"org.sireum.gate.GateMetric","id":"ratio","unit":"fraction","canonicalValue":"0.1"}],"message":"quote:\" reverse:\\ solidus:\/"},{"type":"org.sireum.gate.GateResult","obligation":{"type":"org.sireum.gate.GateObligation","namespace":"gate.example","id":"02-failed"},"outcome":{"type":"org.sireum.gate.GateOutcome.Type","value":"Failed"},"durationMillis":1,"metrics":[{"type":"org.sireum.gate.GateMetric","id":"failures","unit":"count","canonicalValue":"1"}],"message":"short:\b\t\n\f\r"},{"type":"org.sireum.gate.GateResult","obligation":{"type":"org.sireum.gate.GateObligation","namespace":"gate.example","id":"03-skipped"},"outcome":{"type":"org.sireum.gate.GateOutcome.Type","value":"Skipped"},"durationMillis":2,"metrics":[],"message":"not selected"},{"type":"org.sireum.gate.GateResult","obligation":{"type":"org.sireum.gate.GateObligation","namespace":"gate.example","id":"04-timed-out"},"outcome":{"type":"org.sireum.gate.GateOutcome.Type","value":"TimedOut"},"durationMillis":604800000,"metrics":[{"type":"org.sireum.gate.GateMetric","id":"timeout","unit":"millisecond","canonicalValue":"604800000"}],"message":"unicode:é中😀"},{"type":"org.sireum.gate.GateResult","obligation":{"type":"org.sireum.gate.GateObligation","namespace":"gate.example","id":"05-error"},"outcome":{"type":"org.sireum.gate.GateOutcome.Type","value":"Error"},"durationMillis":4,"metrics":[{"type":"org.sireum.gate.GateMetric","id":"errors","unit":"count","canonicalValue":"1.01"}],"message":"combining:é"}],"metrics":[{"type":"org.sireum.gate.GateMetric","id":"artifact-bytes","unit":"byte","canonicalValue":"10"},{"type":"org.sireum.gate.GateMetric","id":"coverage","unit":"percent","canonicalValue":"99.5"}],"diagnostics":[{"type":"org.sireum.gate.GateDiagnostic","severity":{"type":"org.sireum.gate.GateDiagnosticSeverity.Type","value":"Info"},"code":"I001","message":"info\/ok"},{"type":"org.sireum.gate.GateDiagnostic","severity":{"type":"org.sireum.gate.GateDiagnosticSeverity.Type","value":"Warning"},"code":"W001","message":"warning:\"quoted\""},{"type":"org.sireum.gate.GateDiagnostic","severity":{"type":"org.sireum.gate.GateDiagnosticSeverity.Type","value":"Error"},"code":"E001","message":"error:\\fatal"}]}"""

  val EscapingGoldenTree: Predef.String =
    """{"type":"org.sireum.gate.GateDiagnostic","severity":{"type":"org.sireum.gate.GateDiagnosticSeverity.Type","value":"Info"},"code":"edge","message":"\"\\\/""" +
      "\\u0000\\u0001\\u0002\\u0003\\u0004\\u0005\\u0006\\u0007" +
      "\\b\\t\\n\\u000B\\f\\r\\u000E\\u000F" +
      "\\u0010\\u0011\\u0012\\u0013\\u0014\\u0015\\u0016\\u0017" +
      "\\u0018\\u0019\\u001A\\u001B\\u001C\\u001D\\u001E\\u001F" +
      "\\u007F" + """é中😀"}"""
}

class SagaReportTest extends TestSuite {

  import SagaReportWire._

  private def tempRoot[A](f: Path => A): A = {
    val home = Paths.get(System.getenv("SIREUM_HOME")).toAbsolutePath.normalize
    val base = home.resolve("out").resolve("saga-report-test-temp")
    Files.createDirectories(base)
    val root = Files.createTempDirectory(base, "case-")
    try f(root)
    finally deleteTree(root)
  }

  private def deleteTree(root: Path): Unit = {
    if (Files.exists(root, LinkOption.NOFOLLOW_LINKS)) {
      val stream = Files.walk(root)
      try {
        stream.iterator.asScala.toVector.sortBy(_.getNameCount).reverse.foreach(Files.deleteIfExists)
      } finally {
        stream.close()
      }
    }
  }

  private def write(path: Path, text: Predef.String): Unit = {
    Files.createDirectories(path.getParent)
    Files.write(path, text.getBytes(StandardCharsets.UTF_8))
    ()
  }

  private def xmlDir(root: Path): Path = {
    val result = root.resolve("xml")
    Files.createDirectory(result)
    result
  }

  private def emit(root: Path, documents: (Predef.String, Predef.String)*): Predef.String = {
    val dir = xmlDir(root)
    documents.foreach { case (name, content) => write(dir.resolve(name), content) }
    val report = root.resolve("report.json")
    SagaReport_Ext.emitDirectory(dir, report)
    new Predef.String(Files.readAllBytes(report), StandardCharsets.UTF_8)
  }

  private def passingSuiteXml(name: Predef.String, count: Int): Predef.String = {
    val builder = new java.lang.StringBuilder
    builder.append(
      s"""<testsuite name="$name" tests="$count" failures="0" errors="0" time="0">""")
    builder.append("<properties/>")
    var i = 0
    while (i < count) {
      builder.append(
        s"""<testcase name="t$i" classname="$name" time="0"/>""")
      i += 1
    }
    builder.append("<system-out/><system-err/></testsuite>")
    builder.toString
  }

  private def goldenReport: Report = {
    val obligations = Vector(
      Obligation("gate.example", "01-passed"),
      Obligation("gate.example", "02-failed"),
      Obligation("gate.example", "03-skipped"),
      Obligation("gate.example", "04-timed-out"),
      Obligation("gate.example", "05-error"))
    Report(
      producerId = "producer/\"\\edge",
      producerVersion = "v1-é😀",
      selectedObligations = obligations,
      results = Vector(
        Result(
          obligations(0),
          Outcome.Passed,
          0,
          Vector(Metric("attempts", "count", "0"), Metric("ratio", "fraction", "0.1")),
          "quote:\" reverse:\\ solidus:/"),
        Result(
          obligations(1),
          Outcome.Failed,
          1,
          Vector(Metric("failures", "count", "1")),
          "short:\b\t\n\f\r"),
        Result(obligations(2), Outcome.Skipped, 2, Vector.empty, "not selected"),
        Result(
          obligations(3),
          Outcome.TimedOut,
          MaxDurationMillis,
          Vector(Metric("timeout", "millisecond", "604800000")),
          "unicode:é中😀"),
        Result(
          obligations(4),
          Outcome.Error,
          4,
          Vector(Metric("errors", "count", "1.01")),
          "combining:é")),
      metrics = Vector(
        Metric("artifact-bytes", "byte", "10"),
        Metric("coverage", "percent", "99.5")),
      diagnostics = Vector(
        Diagnostic(Severity.Info, "I001", "info/ok"),
        Diagnostic(Severity.Warning, "W001", "warning:\"quoted\""),
        Diagnostic(Severity.Error, "E001", "error:\\fatal")))
  }

  "wire tree matches the frozen cross-repository golden" in tempRoot { root =>
    val report = root.resolve("golden-report.json")
    SagaReport_Ext.publishForTest(renderBytes(goldenReport), report)
    assert(Files.readAllBytes(report).sameElements(
      SagaReportGolden.GoldenTree.getBytes(StandardCharsets.UTF_8)))
  }

  "wire escaping matches the frozen escaping golden" in {
    val controls = (0 to 31).map(_.toChar).mkString
    val diagnostic = Diagnostic(
      Severity.Info,
      "edge",
      "\"\\/" + controls + 0x7f.toChar + "é中😀")
    val report = Report(
      "p",
      "1",
      Vector(Obligation("n", "i")),
      Vector(Result(Obligation("n", "i"), Outcome.Passed, 0, Vector.empty, "")),
      Vector.empty,
      Vector.empty)
    validate(report)
    val builder = new java.lang.StringBuilder
    builder.append("""{"type":"org.sireum.gate.GateDiagnostic","severity":{"type":"org.sireum.gate.GateDiagnosticSeverity.Type","value":""")
    builder.append('"').append(diagnostic.severity.value).append("\"}")
    builder.append(""","code":"edge","message":""")
    val wrapped = Report(
      "p",
      "1",
      Vector(Obligation("n", "i")),
      Vector(Result(Obligation("n", "i"), Outcome.Passed, 0, Vector.empty, diagnostic.message)),
      Vector.empty,
      Vector.empty)
    val rendered = render(wrapped)
    val marker = ""","message":"""
    val messageStart = rendered.indexOf(marker) + marker.length
    val messageEnd = rendered.indexOf("}]", messageStart)
    builder.append(rendered.substring(messageStart, messageEnd))
    builder.append('}')
    assert(builder.toString == SagaReportGolden.EscapingGoldenTree)
  }

  "wire validation rejects noncanonical decimals and Error diagnostics are not green" in {
    val obligation = Obligation("n", "i")
    val bad = Report(
      "p",
      "1",
      Vector(obligation),
      Vector(Result(obligation, Outcome.Passed, 0, Vector.empty, "")),
      Vector(Metric("m", "count", "1.0")),
      Vector.empty)
    assertThrows[IllegalArgumentException](render(bad))
    val errorDiagnostic = Report(
      "p",
      "1",
      Vector(obligation),
      Vector(Result(obligation, Outcome.Passed, 0, Vector.empty, "")),
      Vector.empty,
      Vector(Diagnostic(Severity.Error, "E", "fatal")))
    assert(!isGreen(errorDiagnostic))
  }

  "canonical XML retains pass failure skip error and sorts obligations" in tempRoot { root =>
    val z =
      """<?xml version="1.0" encoding="UTF-8"?>
        |<testsuite name="z.Suite" tests="2" failures="0" errors="0" skipped="1" time="0.006">
        |  <properties/>
        |  <testcase name="passed" classname="z.Suite" time="0.001"/>
        |  <testcase name="skipped" classname="z.Suite" time="0.002"><skipped message="not selected"/></testcase>
        |  <system-out></system-out><system-err></system-err>
        |</testsuite>""".stripMargin
    val a =
      """<?xml version="1.0" encoding="UTF-8"?>
        |<testsuite name="a.Suite" tests="2" failures="1" errors="1" time="0.010">
        |  <properties/>
        |  <testcase name="error" classname="a.Suite" time="0.004"><error message="boom">trace</error></testcase>
        |  <testcase name="failed" classname="a.Suite" time="0.003"><failure message="nope"/></testcase>
        |  <system-out></system-out><system-err></system-err>
        |</testsuite>""".stripMargin
    val actual = emit(root, "TEST-z.xml" -> z, "TEST-a.xml" -> a)
    val obligations = Vector(
      Obligation("a.Suite", "error"),
      Obligation("a.Suite", "failed"),
      Obligation("z.Suite", "passed"),
      Obligation("z.Suite", "skipped"))
    val expected = Report(
      SagaReport_Ext.ProducerId,
      SagaReport_Ext.ProducerVersion,
      obligations,
      Vector(
        Result(obligations(0), Outcome.Error, 4, Vector.empty, "boom\ntrace"),
        Result(obligations(1), Outcome.Failed, 3, Vector.empty, "nope"),
        Result(obligations(2), Outcome.Passed, 1, Vector.empty, ""),
        Result(obligations(3), Outcome.Skipped, 2, Vector.empty, "not selected")),
      Vector.empty,
      Vector.empty)
    assert(actual == render(expected))
  }

  "aborted suite with XML still publishes as Error" in tempRoot { root =>
    val xml =
      """<testsuite name="aborted.Suite" tests="0" failures="0" errors="1" time="0.007">
        |<properties/><system-out></system-out><system-err>initialization failed</system-err>
        |</testsuite>""".stripMargin
    val dir = xmlDir(root)
    write(dir.resolve("TEST-aborted.xml"), xml)
    val report = root.resolve("report.json")
    val emission = SagaReport_Ext.emitAndCleanup(
      String(dir.toString),
      String(report.toString),
      1,
      ISZ("aborted.Suite"))
    assert(emission._1 == 0)
    assert(emission._2 == string"")
    assert(Files.exists(report, LinkOption.NOFOLLOW_LINKS))
    assert(!Files.exists(dir, LinkOption.NOFOLLOW_LINKS))
    val actual = new Predef.String(Files.readAllBytes(report), StandardCharsets.UTF_8)
    val obligation = Obligation("aborted.Suite", "<suite-error>")
    val expected = Report(
      SagaReport_Ext.ProducerId,
      SagaReport_Ext.ProducerVersion,
      Vector(obligation),
      Vector(Result(obligation, Outcome.Error, 7, Vector.empty, "initialization failed")),
      Vector.empty,
      Vector.empty)
    assert(actual == render(expected))
  }

  "XXE DOCTYPE and external entity are rejected without reading the secret" in tempRoot { root =>
    val secret = root.resolve("secret.txt")
    write(secret, "XXE-SECRET-MUST-NOT-APPEAR")
    val xml =
      s"""<?xml version="1.0"?>
         |<!DOCTYPE testsuite [<!ENTITY xxe SYSTEM "${secret.toUri}">]>
         |<testsuite name="xxe.Suite" tests="1" failures="1" errors="0" time="0.001">
         |<properties/><testcase name="hostile" classname="xxe.Suite" time="0.001">
         |<failure>&xxe;</failure></testcase><system-out/><system-err/>
         |</testsuite>""".stripMargin
    val dir = xmlDir(root)
    write(dir.resolve("TEST-xxe.xml"), xml)
    val report = root.resolve("report.json")
    val error = intercept[Exception](SagaReport_Ext.emitDirectory(dir, report))
    assert(!Files.exists(report, LinkOption.NOFOLLOW_LINKS))
    assert(!_root_.scala.Option(error.getMessage).getOrElse("").contains("XXE-SECRET-MUST-NOT-APPEAR"))
  }

  "malformed XML is rejected without a report" in tempRoot { root =>
    val dir = xmlDir(root)
    write(dir.resolve("TEST-bad.xml"), "<testsuite>")
    val report = root.resolve("report.json")
    assertThrows[Exception](SagaReport_Ext.emitDirectory(dir, report))
    assert(!Files.exists(report, LinkOption.NOFOLLOW_LINKS))
  }

  "duplicate obligations across documents are rejected" in tempRoot { root =>
    val xml =
      """<testsuite name="dup.Suite" tests="1" failures="0" errors="0" time="0.001">
        |<properties/><testcase name="same" classname="dup.Suite" time="0.001"/>
        |<system-out/><system-err/></testsuite>""".stripMargin
    val dir = xmlDir(root)
    write(dir.resolve("TEST-a.xml"), xml)
    write(dir.resolve("TEST-b.xml"), xml)
    val report = root.resolve("report.json")
    assertThrows[Exception](SagaReport_Ext.emitDirectory(dir, report))
    assert(!Files.exists(report, LinkOption.NOFOLLOW_LINKS))
  }

  "duplicate suite names with disjoint testcases are rejected" in tempRoot { root =>
    val first =
      """<testsuite name="same.Suite" tests="1" failures="0" errors="0" time="0.001">
        |<properties/><testcase name="first" classname="same.Suite" time="0.001"/>
        |<system-out/><system-err/></testsuite>""".stripMargin
    val second =
      """<testsuite name="same.Suite" tests="1" failures="0" errors="0" time="0.001">
        |<properties/><testcase name="second" classname="same.Suite" time="0.001"/>
        |<system-out/><system-err/></testsuite>""".stripMargin
    val dir = xmlDir(root)
    write(dir.resolve("TEST-a.xml"), first)
    write(dir.resolve("TEST-b.xml"), second)
    val report = root.resolve("report.json")
    val error = intercept[Exception](SagaReport_Ext.emitDirectory(dir, report))
    assert(error.getMessage == "Duplicate ScalaTest suite: same.Suite")
    assert(!Files.exists(report, LinkOption.NOFOLLOW_LINKS))
  }

  "empty present testcase classname is rejected" in tempRoot { root =>
    val xml =
      """<testsuite name="empty.Suite" tests="1" failures="0" errors="0" time="0.001">
        |<properties/><testcase name="test" classname="" time="0.001"/>
        |<system-out/><system-err/></testsuite>""".stripMargin
    val dir = xmlDir(root)
    write(dir.resolve("TEST-empty-classname.xml"), xml)
    val report = root.resolve("report.json")
    assertThrows[Exception](SagaReport_Ext.emitDirectory(dir, report))
    assert(!Files.exists(report, LinkOption.NOFOLLOW_LINKS))
  }

  "completed red run with exit 1 still publishes and cleans XML" in tempRoot { root =>
    val xml =
      """<testsuite name="red.Suite" tests="2" failures="1" errors="0" time="0.004">
        |<properties/>
        |<testcase name="passes" classname="red.Suite" time="0.001"/>
        |<testcase name="fails" classname="red.Suite" time="0.003">
        |<failure message="assertion failed"/>
        |</testcase>
        |<system-out/><system-err/></testsuite>""".stripMargin
    val dir = xmlDir(root)
    write(dir.resolve("TEST-red.xml"), xml)
    val report = root.resolve("report.json")
    val emission = SagaReport_Ext.emitAndCleanup(
      String(dir.toString),
      String(report.toString),
      1,
      ISZ("red.Suite"))
    assert(emission._1 == 0)
    assert(emission._2 == string"")
    assert(Files.exists(report, LinkOption.NOFOLLOW_LINKS))
    val content = new Predef.String(Files.readAllBytes(report), StandardCharsets.UTF_8)
    assert(content.contains(""""value":"Failed""""))
    assert(content.contains(""""namespace":"red.Suite","id":"fails""""))
    assert(!Files.exists(dir, LinkOption.NOFOLLOW_LINKS))
  }

  "saga report refuses selection modes without a complete classes inventory" in {
    def assertRefused(result: (Z, String)): Unit = {
      assert(result._1 == SagaReport.InvalidTarget)
      assert(result._2.value.contains("--classes"))
    }

    assertRefused(Test.validateSagaReportSelection(
      classNames = ISZ(),
      suffixes = ISZ("Suite"),
      packageNames = ISZ(),
      names = ISZ()))
    assertRefused(Test.validateSagaReportSelection(
      classNames = ISZ(),
      suffixes = ISZ(),
      packageNames = ISZ("probe"),
      names = ISZ()))
    assertRefused(Test.validateSagaReportSelection(
      classNames = ISZ(),
      suffixes = ISZ(),
      packageNames = ISZ(),
      names = ISZ("probe")))
    assertRefused(Test.validateSagaReportSelection(
      classNames = ISZ(),
      suffixes = ISZ(),
      packageNames = ISZ(),
      names = ISZ()))
    assertRefused(Test.validateSagaReportSelection(
      classNames = ISZ("probe.FailSuite"),
      suffixes = ISZ("Suite"),
      packageNames = ISZ(),
      names = ISZ()))
    assertRefused(Test.validateSagaReportSelection(
      classNames = ISZ("  "),
      suffixes = ISZ(),
      packageNames = ISZ(),
      names = ISZ()))

    assert(Test.validateSagaReportSelection(
      classNames = ISZ(" probe.FailSuite ", "probe.InitBoomSuite"),
      suffixes = ISZ(),
      packageNames = ISZ(),
      names = ISZ()) == (0, String("")))
  }

  "requested suite missing its testsuite refuses a truncated red session" in tempRoot { root =>
    val xml =
      """<testsuite name="probe.FailSuite" tests="1" failures="1" errors="0" time="0.001">
        |<properties/><testcase name="fails" classname="probe.FailSuite" time="0.001">
        |<failure message="ordinary failure"/></testcase><system-out/><system-err/>
        |</testsuite>""".stripMargin
    val dir = xmlDir(root)
    write(dir.resolve("TEST-probe.FailSuite.xml"), xml)
    val report = root.resolve("report.json")
    val emission = SagaReport_Ext.emitAndCleanup(
      String(dir.toString),
      String(report.toString),
      1,
      ISZ("probe.FailSuite", "probe.InitBoomSuite"))
    assert(emission._1 == SagaReport_Ext.EmissionError)
    assert(emission._2.value.contains("produced no testsuite for requested suite"))
    assert(emission._2.value.contains("probe.InitBoomSuite"))
    assert(emission._2.value.contains("refusing saga report publication"))
    assert(!Files.exists(report, LinkOption.NOFOLLOW_LINKS))
    assert(!Files.exists(dir, LinkOption.NOFOLLOW_LINKS))
  }

  "completed red run truncates an oversized message and publishes a diagnostic" in tempRoot { root =>
    val oversized = "Y" * (MaxMessageUtf8Bytes + 1)
    val xml =
      s"""<testsuite name="probe.BigMsgSuite" tests="1" failures="1" errors="0" time="0.001">
         |<properties/><testcase name="bigfail" classname="probe.BigMsgSuite" time="0.001">
         |<failure message="$oversized"/></testcase><system-out/><system-err/>
         |</testsuite>""".stripMargin
    val dir = xmlDir(root)
    write(dir.resolve("TEST-probe.BigMsgSuite.xml"), xml)
    val report = root.resolve("report.json")
    val emission = SagaReport_Ext.emitAndCleanup(
      String(dir.toString),
      String(report.toString),
      1,
      ISZ("probe.BigMsgSuite"))
    assert(emission._1 == 0)
    assert(emission._2 == string"")
    assert(Files.exists(report, LinkOption.NOFOLLOW_LINKS))
    assert(!Files.exists(dir, LinkOption.NOFOLLOW_LINKS))
    val obligation = Obligation("probe.BigMsgSuite", "bigfail")
    val expected = Report(
      SagaReport_Ext.ProducerId,
      SagaReport_Ext.ProducerVersion,
      Vector(obligation),
      Vector(Result(
        obligation,
        Outcome.Failed,
        1,
        Vector.empty,
        "Y" * MaxMessageUtf8Bytes)),
      Vector.empty,
      Vector(Diagnostic(
        Severity.Error,
        "crucible.test.message.invalid",
        "test 'probe.BigMsgSuite::bigfail' emitted an invalid or oversized message")))
    val actual = new Predef.String(Files.readAllBytes(report), StandardCharsets.UTF_8)
    assert(actual == render(expected))
  }

  "abnormal ScalaTest exit refuses a partial green report non-silently and cleans XML" in tempRoot { root =>
    val dir = xmlDir(root)
    write(dir.resolve("TEST-partial.xml"), passingSuiteXml("partial.Suite", 1))
    val report = root.resolve("report.json")
    val emission = SagaReport_Ext.emitAndCleanup(
      String(dir.toString),
      String(report.toString),
      137,
      ISZ("partial.Suite"))
    assert(emission._1 == SagaReport_Ext.EmissionError)
    assert(emission._2.value.contains("exit code 137"))
    assert(emission._2.value.contains("refusing saga report publication"))
    assert(!Files.exists(report, LinkOption.NOFOLLOW_LINKS))
    assert(!Files.exists(dir, LinkOption.NOFOLLOW_LINKS))
  }

  "exit 1 without a failed or error outcome is refused non-silently" in tempRoot { root =>
    val dir = xmlDir(root)
    write(dir.resolve("TEST-partial.xml"), passingSuiteXml("partial.Suite", 1))
    val report = root.resolve("report.json")
    val emission = SagaReport_Ext.emitAndCleanup(
      String(dir.toString),
      String(report.toString),
      1,
      ISZ("partial.Suite"))
    assert(emission._1 == SagaReport_Ext.EmissionError)
    assert(emission._2.value.contains("exit code 1 has no Failed or Error XML outcome"))
    assert(emission._2.value.contains("refusing saga report publication"))
    assert(!Files.exists(report, LinkOption.NOFOLLOW_LINKS))
    assert(!Files.exists(dir, LinkOption.NOFOLLOW_LINKS))
  }

  "incomplete XML counts are rejected" in tempRoot { root =>
    val xml =
      """<testsuite name="incomplete.Suite" tests="2" failures="0" errors="0" time="0.001">
        |<properties/><testcase name="only" classname="incomplete.Suite" time="0.001"/>
        |<system-out/><system-err/></testsuite>""".stripMargin
    val dir = xmlDir(root)
    write(dir.resolve("TEST-incomplete.xml"), xml)
    val report = root.resolve("report.json")
    val emission = SagaReport_Ext.emitAndCleanup(
      String(dir.toString),
      String(report.toString),
      1,
      ISZ("incomplete.Suite"))
    assert(emission._1 == SagaReport_Ext.EmissionError)
    assert(emission._2.value.contains("JUnit tests count 2 does not match 1 testcases"))
    assert(!Files.exists(report, LinkOption.NOFOLLOW_LINKS))
    assert(!Files.exists(dir, LinkOption.NOFOLLOW_LINKS))

    val envelopeDir = root.resolve("incomplete-envelope")
    Files.createDirectory(envelopeDir)
    val incompleteEnvelope =
      """<testsuite name="incomplete.Envelope" tests="1" failures="0" errors="0" time="0.001">
        |<testcase name="only" classname="incomplete.Envelope" time="0.001"/>
        |</testsuite>""".stripMargin
    write(envelopeDir.resolve("TEST-incomplete-envelope.xml"), incompleteEnvelope)
    assertThrows[Exception](SagaReport_Ext.emitDirectory(envelopeDir, report))
    assert(!Files.exists(report, LinkOption.NOFOLLOW_LINKS))
  }

  "overflowing declared count without matching testcases is rejected structurally" in tempRoot { root =>
    val xml =
      s"""<testsuite name="overflow.Suite" tests="${MaxSelectedObligations + 1}"
         | failures="0" errors="0" time="0"><properties/><system-out/><system-err/></testsuite>""".stripMargin
    val dir = xmlDir(root)
    write(dir.resolve("TEST-overflow.xml"), xml)
    val report = root.resolve("report.json")
    val error = intercept[Exception](SagaReport_Ext.emitDirectory(dir, report))
    assert(error.getMessage ==
      s"JUnit tests count ${MaxSelectedObligations + 1} does not match 0 testcases")
    assert(!Files.exists(report, LinkOption.NOFOLLOW_LINKS))
  }

  "matching testcase count overflow is clamped with an Error diagnostic" in tempRoot { root =>
    val dir = xmlDir(root)
    write(
      dir.resolve("TEST-overflow-matching.xml"),
      passingSuiteXml("overflow.Suite", MaxSelectedObligations + 1))
    val report = root.resolve("report.json")
    SagaReport_Ext.emitDirectory(dir, report)
    assert(Files.exists(report, LinkOption.NOFOLLOW_LINKS))
    val actual = new Predef.String(Files.readAllBytes(report), StandardCharsets.UTF_8)
    assert(actual.contains(""""code":"crucible.session.count.overflow""""))
    assert(actual.contains(s""""namespace":"overflow.Suite","id":"t${MaxSelectedObligations - 1}""""))
    assert(!actual.contains(s""""namespace":"overflow.Suite","id":"t$MaxSelectedObligations""""))
  }

  "out-of-range durations are clamped with Error diagnostics" in tempRoot { root =>
    val xml =
      """<testsuite name="duration.Suite" tests="2" failures="0" errors="0" time="0">
        |<properties/>
        |<testcase name="negative" classname="duration.Suite" time="-0.001"/>
        |<testcase name="slow" classname="duration.Suite" time="604800.001"/>
        |<system-out/><system-err/></testsuite>""".stripMargin
    val dir = xmlDir(root)
    write(dir.resolve("TEST-duration.xml"), xml)
    val report = root.resolve("report.json")
    SagaReport_Ext.emitDirectory(dir, report)
    val negative = Obligation("duration.Suite", "negative")
    val slow = Obligation("duration.Suite", "slow")
    val expected = Report(
      SagaReport_Ext.ProducerId,
      SagaReport_Ext.ProducerVersion,
      Vector(negative, slow),
      Vector(
        Result(negative, Outcome.Passed, 0, Vector.empty, ""),
        Result(slow, Outcome.Passed, MaxDurationMillis, Vector.empty, "")),
      Vector.empty,
      Vector(
        Diagnostic(
          Severity.Error,
          "crucible.test.duration.invalid",
          "test 'duration.Suite::negative' reported a negative duration"),
        Diagnostic(
          Severity.Error,
          "crucible.test.duration.invalid",
          "test 'duration.Suite::slow' exceeded the duration bound")))
    val actual = new Predef.String(Files.readAllBytes(report), StandardCharsets.UTF_8)
    assert(actual == render(expected))
  }

  "nested traversal-like XML entries are rejected" in tempRoot { root =>
    val dir = xmlDir(root)
    val nested = dir.resolve("nested.xml")
    Files.createDirectory(nested)
    write(nested.resolve("TEST-hidden.xml"), "<testsuite/>")
    val report = root.resolve("report.json")
    assertThrows[Exception](SagaReport_Ext.emitDirectory(dir, report))
    assert(!Files.exists(report, LinkOption.NOFOLLOW_LINKS))
  }

  "symlink XML documents are rejected" in tempRoot { root =>
    val outside = root.resolve("outside.xml")
    write(outside, "<testsuite/>")
    val dir = xmlDir(root)
    Files.createSymbolicLink(dir.resolve("TEST-link.xml"), outside)
    val report = root.resolve("report.json")
    assertThrows[Exception](SagaReport_Ext.emitDirectory(dir, report))
    assert(!Files.exists(report, LinkOption.NOFOLLOW_LINKS))
  }

  "existing report targets are rejected" in tempRoot { root =>
    val report = root.resolve("report.json")
    write(report, "existing")
    assertThrows[Exception](SagaReport_Ext.validateTargetForTest(report))
    assert(new Predef.String(Files.readAllBytes(report), StandardCharsets.UTF_8) == "existing")
  }

  "symlink report targets are rejected" in tempRoot { root =>
    val actual = root.resolve("actual.json")
    write(actual, "existing")
    val report = root.resolve("report.json")
    Files.createSymbolicLink(report, actual)
    assertThrows[Exception](SagaReport_Ext.validateTargetForTest(report))
    assert(new Predef.String(Files.readAllBytes(actual), StandardCharsets.UTF_8) == "existing")
  }

  "symlink report parent components are rejected" in tempRoot { root =>
    val actual = root.resolve("actual-parent")
    Files.createDirectory(actual)
    val linked = root.resolve("linked-parent")
    Files.createSymbolicLink(linked, actual)
    assertThrows[Exception](SagaReport_Ext.validateTargetForTest(linked.resolve("report.json")))
    assert(!Files.exists(actual.resolve("report.json"), LinkOption.NOFOLLOW_LINKS))
  }

  "deep symlink report parent components are rejected" in tempRoot { root =>
    val actualGrandparent = root.resolve("actual-grandparent")
    val actualParent = actualGrandparent.resolve("real-parent")
    Files.createDirectories(actualParent)
    val linkedGrandparent = root.resolve("linked-grandparent")
    Files.createSymbolicLink(linkedGrandparent, actualGrandparent)
    val report = linkedGrandparent.resolve("real-parent").resolve("report.json")
    assertThrows[Exception](SagaReport_Ext.validateTargetForTest(report))
    assert(!Files.exists(actualParent.resolve("report.json"), LinkOption.NOFOLLOW_LINKS))
  }

  "traversal report paths are rejected" in tempRoot { root =>
    val traversing = root.resolve("child").resolve("..").resolve("report.json")
    assertThrows[Exception](SagaReport_Ext.validateTargetForTest(traversing))
    assert(!Files.exists(root.resolve("report.json"), LinkOption.NOFOLLOW_LINKS))
  }

  "atomic interruption leaves neither target nor temporary report" in tempRoot { root =>
    val report = root.resolve("report.json")
    assertThrows[RuntimeException](
      SagaReport_Ext.publishForTest(
        "complete".getBytes(StandardCharsets.UTF_8),
        report,
        _ => throw new RuntimeException("interrupted before atomic link")))
    assert(!Files.exists(report, LinkOption.NOFOLLOW_LINKS))
    val stream = Files.list(root)
    try {
      assert(!stream.iterator.asScala.exists(_.getFileName.toString.endsWith(".tmp")))
    } finally {
      stream.close()
    }
  }

  "atomic publication is complete and never replaces an existing report" in tempRoot { root =>
    val report = root.resolve("report.json")
    val first = "first-complete".getBytes(StandardCharsets.UTF_8)
    SagaReport_Ext.publishForTest(first, report)
    assert(Files.readAllBytes(report).sameElements(first))
    assertThrows[Exception](
      SagaReport_Ext.publishForTest("second".getBytes(StandardCharsets.UTF_8), report))
    assert(Files.readAllBytes(report).sameElements(first))
  }

  "concurrent test launches use distinct argument files" in {
    val left = Test.freshArgFile("scala-test")
    val right = Test.freshArgFile("scala-test")
    try {
      left.writeOver("left")
      right.writeOver("right")
      assert(left != right)
      assert(left.read == string"left")
      assert(right.read == string"right")
    } finally {
      left.removeAll()
      right.removeAll()
    }
  }

  "no-report ScalaTest args match the pre-wave bytes and opt-in adds one XML reporter" in tempRoot { root =>
    val before = Files.list(root)
    val beforeNames =
      try before.iterator.asScala.map(_.getFileName.toString).toVector
      finally before.close()
    val args = Test.scalaTestArgs(
      args = ISZ("-ea", "-classpath", "cp"),
      parTest = F,
      testClasspath = ISZ("/a path", "/b"),
      classNames = ISZ("a.Test"),
      suffixes = ISZ("Suite"),
      packageNames = ISZ("a.pkg"),
      names = ISZ("root.pkg"),
      tests = ISZ("focused test"),
      sagaXmlDirOpt = None())
    val actual = Test.argFileContent(args).value.getBytes(StandardCharsets.UTF_8)
    val preWave =
      """-ea
        |-classpath
        |cp
        |org.scalatest.tools.Runner
        |-C
        |org.sireum.test.ScalaTestReporter
        |-P1
        |-R
        |"/a path /b"
        |-s
        |a.Test
        |-q
        |Suite
        |-m
        |a.pkg
        |-w
        |root.pkg
        |-z
        |"focused test"""".stripMargin.getBytes(StandardCharsets.UTF_8)
    assert(actual.sameElements(preWave))
    val after = Files.list(root)
    val afterNames =
      try after.iterator.asScala.map(_.getFileName.toString).toVector
      finally after.close()
    assert(beforeNames == afterNames)

    val withReport = Test.scalaTestArgs(
      args = ISZ("-ea", "-classpath", "cp"),
      parTest = F,
      testClasspath = ISZ("/a path", "/b"),
      classNames = ISZ("a.Test"),
      suffixes = ISZ(),
      packageNames = ISZ(),
      names = ISZ(),
      tests = ISZ(),
      sagaXmlDirOpt = Some("/private/xml dir"))
    assert(withReport.elements.count(_._1 == string"-u") == 1)
    val index = withReport.elements.indexWhere(_._1 == string"-u")
    assert(withReport(index + 1) == (String("/private/xml dir"), T))
  }
}

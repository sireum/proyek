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
package org.sireum.proyek;

import org.junit.platform.engine.discovery.ClassNameFilter;
import org.junit.platform.engine.discovery.DiscoverySelectors;
import org.junit.platform.engine.discovery.PackageNameFilter;
import org.junit.platform.launcher.EngineFilter;
import org.junit.platform.launcher.Launcher;
import org.junit.platform.launcher.LauncherDiscoveryRequest;
import org.junit.platform.launcher.core.LauncherDiscoveryRequestBuilder;
import org.junit.platform.launcher.core.LauncherFactory;
import org.junit.platform.launcher.listeners.SummaryGeneratingListener;
import org.junit.platform.launcher.listeners.TestExecutionSummary;

import java.io.PrintWriter;
import java.nio.file.Path;
import java.util.*;

/**
 * Runs JUnit Platform tests by scanning classpath roots for registered
 * {@link org.junit.platform.engine.TestEngine} implementations.
 *
 * <p>Excludes "scalatest" (proyek runs ScalaTest via its own runner) and
 * "junit-jupiter" (standard JUnit 5 {@code @Test} not used in Slang projects).
 *
 * <p>Usage: {@code java ... org.sireum.proyek.JUnit5Runner [options] <dir> ...}
 *
 * <p>Options:
 * <ul>
 *   <li>{@code -s <className>} — select a specific fully-qualified test class</li>
 *   <li>{@code -m <packageName>} — select a specific package</li>
 *   <li>{@code -q <suffix>} — include only classes whose name ends with suffix</li>
 *   <li>{@code -w <prefix>} — include only classes in packages starting with prefix</li>
 * </ul>
 *
 * <p>Remaining arguments are classpath root directories to scan.
 * Exit code is 1 if any test fails, 0 otherwise.
 */
public final class JUnit5Runner {

    public static void main(String[] args) {
        if (args.length == 0) {
            return;
        }

        List<String> classNames = new ArrayList<>();
        List<String> packageNames = new ArrayList<>();
        List<String> suffixes = new ArrayList<>();
        List<String> prefixes = new ArrayList<>();
        Set<Path> roots = new HashSet<>();

        for (int i = 0; i < args.length; i++) {
            switch (args[i]) {
                case "-s" -> { if (++i < args.length) classNames.add(args[i]); }
                case "-m" -> { if (++i < args.length) packageNames.add(args[i]); }
                case "-q" -> { if (++i < args.length) suffixes.add(args[i]); }
                case "-w" -> { if (++i < args.length) prefixes.add(args[i]); }
                default -> {
                    Path p = Path.of(args[i]);
                    if (java.nio.file.Files.isDirectory(p)) roots.add(p);
                }
            }
        }
        if (roots.isEmpty()) {
            return;
        }

        var builder = LauncherDiscoveryRequestBuilder.request()
                .filters(EngineFilter.excludeEngines("scalatest", "junit-jupiter"));

        // When specific classes are selected, use class selectors instead of classpath roots
        if (!classNames.isEmpty()) {
            for (String cn : classNames) {
                builder.selectors(DiscoverySelectors.selectClass(cn));
            }
        } else {
            builder.selectors(DiscoverySelectors.selectClasspathRoots(roots));
        }

        // Package selectors (additive with classpath roots)
        for (String pkg : packageNames) {
            builder.selectors(DiscoverySelectors.selectPackage(pkg));
        }

        // Class name suffix filter
        if (!suffixes.isEmpty()) {
            String[] patterns = suffixes.stream()
                    .map(s -> ".*" + java.util.regex.Pattern.quote(s))
                    .toArray(String[]::new);
            builder.filters(ClassNameFilter.includeClassNamePatterns(patterns));
        }

        // Package prefix filter
        if (!prefixes.isEmpty()) {
            builder.filters(PackageNameFilter.includePackageNames(prefixes));
        }

        LauncherDiscoveryRequest request = builder.build();
        Launcher launcher = LauncherFactory.create();
        SummaryGeneratingListener listener = new SummaryGeneratingListener();
        launcher.execute(request, listener);

        TestExecutionSummary summary = listener.getSummary();
        if (summary.getTestsFoundCount() > 0) {
            summary.printTo(new PrintWriter(System.out, true));
            if (summary.getTotalFailureCount() > 0) {
                summary.printFailuresTo(new PrintWriter(System.err, true));
                System.exit(1);
            }
        }
    }
}

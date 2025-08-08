package com.woutwerkman.sink.compiler.plugin

import com.woutwerkman.sink.compiler.plugin.runners.AbstractJvmOutputTest
import com.woutwerkman.sink.compiler.plugin.runners.AbstractJvmDiagnosticTest
import org.jetbrains.kotlin.generators.generateTestGroupSuiteWithJUnit5

fun main() {
    generateTestGroupSuiteWithJUnit5 {
        testGroup(testDataRoot = "compiler-plugin/testData", testsRoot = "compiler-plugin/test-gen") {
            testClass<AbstractJvmDiagnosticTest> {
                model("diagnostics")
            }

            testClass<AbstractJvmOutputTest> {
                model("outputTests")
            }
        }
    }
}

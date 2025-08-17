pluginManagement {
    repositories {
        mavenCentral()
        gradlePluginPortal()
        mavenLocal()
    }
}

plugins {
    id("org.gradle.toolchains.foojay-resolver-convention") version "1.0.0"
}

dependencyResolutionManagement {
    repositories {
        mavenCentral()
        mavenLocal()
    }
}

rootProject.name = "Sink"

include("compiler-plugin")
include("gradle-plugin")
include("plugin-annotations")
include("ide-and-compiler-plugin-common-code")
include("example-project")
include("sink-intellij-plugin")

pluginManagement {
    repositories {
        mavenCentral()
        gradlePluginPortal()
    }
}

dependencyResolutionManagement {
    repositories {
        mavenCentral()
    }
}

rootProject.name = "Sink"

include("compiler-plugin")
include("gradle-plugin")
include("plugin-annotations")
include("ide-and-compiler-plugin-common-code")

pluginManagement {
    repositories {
        mavenCentral()
        gradlePluginPortal()
        mavenLocal()
    }
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

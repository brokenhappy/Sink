pluginManagement {
    repositories {
        mavenLocal()
        mavenCentral()
        gradlePluginPortal()
    }
}

dependencyResolutionManagement {
    repositories {
        mavenLocal()
        mavenCentral()
    }
}

rootProject.name = "Sink"

include("compiler-plugin")
include("gradle-plugin")
include("plugin-annotations")
include("ide-and-compiler-plugin-common-code")

include("example-project")
plugins {
    kotlin("multiplatform") version "2.2.0" apply false
    kotlin("jvm") version "2.2.0" apply false
    id("com.github.gmazzo.buildconfig") version "5.6.5"
    id("org.jetbrains.kotlinx.binary-compatibility-validator") version "0.16.3" apply false
}

allprojects {
    group = "com.woutwerkman.sink"
    version = "0.1.0-SNAPSHOT"
}

// Minimal, flexible publishing setup for all modules
subprojects {
    plugins.withId("java") {
        // Ensure sources/javadoc jars for JVM/Java modules
        extensions.configure<org.gradle.api.plugins.JavaPluginExtension>("java") {
            withSourcesJar()
            // Javadoc jar may be empty for Kotlin, but harmless
            withJavadocJar()
        }
    }

    // Apply maven-publish where it makes sense
    plugins.withId("org.jetbrains.kotlin.jvm") {
        pluginManager.apply("maven-publish")
    }
    plugins.withId("org.jetbrains.kotlin.multiplatform") {
        pluginManager.apply("maven-publish")
    }
    plugins.withId("java-gradle-plugin") {
        // java-gradle-plugin registers pluginMaven publication; we also apply maven-publish
        pluginManager.apply("maven-publish")
    }

    // Configure publications after evaluation to ensure components are available
    afterEvaluate {
        extensions.findByType(org.gradle.api.publish.PublishingExtension::class.java)?.apply {
            publications {
                // For JVM/Java modules, create a mavenJava publication if a Java component exists
                val hasJavaComponent = components.findByName("java") != null
                if (hasJavaComponent && findByName("mavenJava") == null) {
                    create("mavenJava", org.gradle.api.publish.maven.MavenPublication::class.java) {
                        from(components.getByName("java"))
                        pom {
                            name.set(project.name)
                            description.set((project.description ?: System.getProperty("POM_DESCRIPTION")) ?: project.name)
                            val projectUrl = (findProperty("POM_URL") as String?) ?: "https://github.com/woutwerkman/Sink"
                            url.set(projectUrl)
                            licenses {
                                license {
                                    this.name.set("The Apache License, Version 2.0")
                                    this.url.set("https://www.apache.org/licenses/LICENSE-2.0.txt")
                                }
                            }
                            scm {
                                val scmUrl = (findProperty("POM_SCM_URL") as String?) ?: projectUrl
                                url.set(scmUrl)
                                connection.set((findProperty("POM_SCM_CONNECTION") as String?) ?: "scm:git:git://github.com/woutwerkman/Sink.git")
                                developerConnection.set((findProperty("POM_SCM_DEV_CONNECTION") as String?) ?: "scm:git:ssh://github.com/woutwerkman/Sink.git")
                            }
                            developers {
                                developer {
                                    id.set((findProperty("POM_DEVELOPER_ID") as String?) ?: "woutwerkman")
                                    name.set((findProperty("POM_DEVELOPER_NAME") as String?) ?: "Wout Werkman")
                                }
                            }
                        }
                    }
                }

                withType(org.gradle.api.publish.maven.MavenPublication::class.java).configureEach {
                    if (!pom.name.isPresent) pom.name.set(project.name)
                    if (!pom.description.isPresent) pom.description.set(project.description ?: project.name)
                    if (!pom.url.isPresent) pom.url.set((findProperty("POM_URL") as String?) ?: "https://github.com/woutwerkman/Sink")
                }
            }

            repositories {
                // Always allow publishing to Maven Local
                mavenLocal()

                // If Sonatype credentials are present, configure OSSRH endpoints
                val ossrhUser = findProperty("ossrhUsername") as String?
                val ossrhPass = findProperty("ossrhPassword") as String?
                val releasesRepo = (findProperty("RELEASES_REPO_URL") as String?)
                    ?: "https://s01.oss.sonatype.org/service/local/staging/deploy/maven2/"
                val snapshotsRepo = (findProperty("SNAPSHOTS_REPO_URL") as String?)
                    ?: "https://s01.oss.sonatype.org/content/repositories/snapshots/"
                val isSnapshot = version.toString().endsWith("SNAPSHOT")
                if (!ossrhUser.isNullOrBlank() && !ossrhPass.isNullOrBlank()) {
                    maven {
                        name = "sonatype"
                        url = uri(if (isSnapshot) snapshotsRepo else releasesRepo)
                        credentials {
                            username = ossrhUser
                            password = ossrhPass
                        }
                    }
                }

                // Optional generic repository via properties
                val genericUrl = findProperty("PUBLISH_URL") as String?
                val genericUser = findProperty("PUBLISH_USERNAME") as String?
                val genericPass = findProperty("PUBLISH_PASSWORD") as String?
                if (!genericUrl.isNullOrBlank()) {
                    maven {
                        name = "custom"
                        url = uri(genericUrl)
                        if (!genericUser.isNullOrBlank() && !genericPass.isNullOrBlank()) {
                            credentials {
                                username = genericUser
                                password = genericPass
                            }
                        }
                    }
                }
            }
        }
    }

    // Optionally apply signing if configured (uses standard Gradle signing properties)
    if (hasProperty("signing.keyId") || hasProperty("signing.gnupg.keyName") || hasProperty("signingKey")) {
        pluginManager.apply("signing")
        extensions.findByType(org.gradle.plugins.signing.SigningExtension::class.java)?.apply {
            val publishing = extensions.findByType(org.gradle.api.publish.PublishingExtension::class.java)
            if (publishing != null) {
                sign(publishing.publications)
            }
        }
    }
}

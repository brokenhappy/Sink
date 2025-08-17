plugins {
    kotlin("jvm") version "2.2.0"
    id("com.woutwerkman.sink") version "0.1.0-SNAPSHOT"
}

group = "com.woutwerkman.sink"
version = "0.1.0-SNAPSHOT"

repositories {
    mavenLocal()
    mavenCentral()
}

dependencies {
    implementation("com.woutwerkman.sink:plugin-annotations:0.1.0-SNAPSHOT")
    testImplementation(kotlin("test"))
}

tasks.test {
    useJUnitPlatform()
}
kotlin {
    jvmToolchain(23)
}
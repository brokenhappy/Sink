package org.jetbrains.kotlin.compiler.plugin.template

@Target(AnnotationTarget.CLASS)
@Retention(AnnotationRetention.BINARY)
@Deprecated("Unless you are a compiler, you don't want to touch this :)")
public annotation class _SinkMetadata(val encoded: String)

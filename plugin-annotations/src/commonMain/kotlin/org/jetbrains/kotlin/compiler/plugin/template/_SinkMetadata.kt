package org.jetbrains.kotlin.compiler.plugin.template

@Target(AnnotationTarget.CLASS)
@Retention(AnnotationRetention.BINARY)
@RequiresOptIn("Unless you are a compiler, you don't want to touch this :)", RequiresOptIn.Level.ERROR)
public annotation class _SinkMetadata(val data: String)

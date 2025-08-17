// MODULE: sink
// FILE: sink.kt
package com.woutwerkman.sink

@Target(AnnotationTarget.FUNCTION, AnnotationTarget.CONSTRUCTOR, AnnotationTarget.CLASS)
@Retention(AnnotationRetention.SOURCE)
public annotation class Injectable(
    val visibility: String = "",
)

public abstract class InjectionCache {
    public companion object {
        public operator fun invoke(): InjectionCache = object : InjectionCache() { // TODO: Make thread safe!
            private val cache = mutableMapOf<Any?, Any?>()
            override fun <T> computeIfAbsent(key: Any?, compute: () -> T): T =
                cache.getOrPut(key, compute) as T
        }
    }
    // TODO: Investigate whether it's worth adding `InjectionCache` receiver, it might reduce the number of fields in the [compute] lambda object
    public abstract fun <T> computeIfAbsent(key: Any?, compute: () -> T): T

    public inline fun <reified T> get(vararg args: Any?): T =
        error("If you're seeing this error, it means you accidentally forgot to compile this module with the Sink compiler plugin.")
}

// MODULE: modulea(sink)
// FILE: fooAndBar.kt
package modulea

import com.woutwerkman.sink.Injectable

interface Foo {
    fun getCharA(): Char
}
interface Bar {
    fun combine(lhs: Char, rhs: Char): String
}
interface Baz {
    fun combine(lhs: Char, rhs: Char): String
}
interface Buz

@Injectable
fun foo(): Foo = object : Foo {
    override fun getCharA(): Char = 'o'
}

@Injectable
fun bar(foo: Foo, baz: Baz, buz: Buz): Bar = object : Bar {
    override fun combine(lhs: Char, rhs: Char): String = baz.combine(lhs, rhs)
}

// MODULE: main(modulea, sink)
// FILE: bazFoobsAndBarbs.kt
package moduleb

import com.woutwerkman.sink.Injectable
import com.woutwerkman.sink.InjectionCache
import modulea.Foo
import modulea.Bar
import modulea.Baz
import modulea.Buz

interface Foobs {
    fun getCharA(): Char
}
interface Barbs {
    fun getText(): String
}
interface Bazbs {
    fun getCharB(): Char
}

@Injectable
fun baz(foo: Foo, bazbs: Bazbs): Baz = object : Baz {
    override fun combine(lhs: Char, rhs: Char): String = "$lhs$rhs"
}

@Injectable
fun foobs(foo: Foo): Foobs = object : Foobs {
    override fun getCharA(): Char = foo.getCharA().uppercaseChar()
}

@Injectable
fun barbs(bar: Bar, foobs: Foobs, bazbs: Bazbs): Barbs = object : Barbs {
    override fun getText(): String = bar.combine(foobs.getCharA(), bazbs.getCharB())
}

fun box(): String = InjectionCache().get<Barbs>(
    object: Bazbs {
        override fun getCharB(): Char = 'K'
    },
    object: Buz {},
).getText()

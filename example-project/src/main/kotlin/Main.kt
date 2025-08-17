package com.woutwerkman.sink.example.project

import com.woutwerkman.sink.Injectable
import com.woutwerkman.sink.InjectionCache

interface Foo {
    fun foo(): String
}

@Injectable
fun foo(): Foo = object: Foo {
    override fun foo(): String = "aaaaaah"
}

fun main() {
    InjectionCache.invoke().get<Foo>().foo().also(::println)
}
package com.woutwerkman.sink.example.project

import com.woutwerkman.sink.Injectable
import com.woutwerkman.sink.InjectionCache

interface Foo

@Injectable
fun foo(): Foo = TODO()

fun main() {
    InjectionCache.invoke().get<Foo>()
}
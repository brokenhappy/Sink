# Sink, making DI code fabulous again!

This DI (Dependency Injection) Framework uses Kotlin's modern syntax to improve over Java-based DI UX.

## Using Sink

Sorry, I haven't published any tag yet. If you'd like to try an early version, please let me know :).

The DI framework with the closest UX is currently: [Inject by IVIanuu](https://github.com/IVIanuu/injekt)

## Core principles

 - Don't use the UX of existing DI frameworks as a base
 - Focus on code that people write when they don't use DI frameworks (but do use DI) and generate their boilerplate.
 - Least magic principle.
 - Don't sacrifice on:
   - Simplicity: Solve as many problems with as few DI features as possible.
   - Architecture: Respect isolation and promote modularization and decoupling.
   - Performance: Decisions are made at compile time, and instantiation is lazy by default.
   - Developer Experience: Catch errors early, actionable errors, and deep integration with the IDE.

## Features

### Core feature

The core feature is a function or class annotated with `@com.woutwerkman.sink.Injectable`.
```kt
@Injectable
fun serviceCreator(): Service = TODO()
```
Or:
```kt
@Injectable
class ServiceImpl: Service
```
After this you can inject `Service` in any other Injectable, as long as it's in the same module or a depending module.
```kt
@Injectable
internal class AppImpl(val service: Service): App

fun main() {
    val app = InjectionCache().get<App>()
}
```
Hmm, what this injection cache is, you ask?
Good question :). First, all `@Injectables` are only singletons.
And as part of the "least magic principle," Sink will not magically store these instances.
So this means that `@Injectables` are only singletons in any injection cache.

A lot more to talk about, but so little time! I'll add more soon!

[//]: # (TODO: Add more)
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
   - Simplicity: Solve as many problems with as few DI features as possible, while keeping features focused.
   - Architecture: Respect isolation and promote modularization and decoupling.
   - Performance: Decisions are made at compile time, and instantiation is lazy by default.
   - Developer Experience: Catch errors early, actionable errors, and deep integration with the IDE.

## Features

### Core feature

The core feature of Sink is the injectable.
An Injectable is uniquely identified by its type.
That means you cannot qualify nor scope it.
Instead we use use modern Kotlin features to achieve scoped and qualified behavior.

You can declare an injectable function or class annotated with `@com.woutwerkman.sink.Injectable`.
```kt
@Injectable // Declares a public injectable of type `Service`
fun serviceCreator(): Service = TODO()
```
Or:
```kt
@Injectable // Declares a public injectable of type `ServiceImpl` and via supertype `Service`
class ServiceImpl: Service
```
After this you can inject `Service` in any other Injectable, as long as it's in the same module or a depending module.
```kt
@Injectable
internal class AppImpl(val service: Service/* This get's autowired by DI framework */): App

fun main() {
    val app = InjectionCache().get<App>()
}
```
Hmm, what this injection cache is, you ask?
Good question :). First, all `@Injectables` are only singletons.
And as part of the "least magic principle," Sink will not magically store these instances.
So this means that `@Injectables` are only singletons in any injection cache.

## External dependencies

It's common for dependencies to be satisfied inside the module or from dependencies of the module.
But sometimes a dependency is **intentionally** left to be implemented by depending modules.

In short:

> If your module *must not* decide which implementation to use, then it can be left up to whoever uses it.

This is an important feature for modularization and decoupling.

### How to declare an external dependency

You don’t need to mark something as external — in fact, you **can’t** (yet).
External dependencies are **implicitly** identified when a dependency cannot be resolved within the same module.
If it’s resolvable inside the module, it’s considered internal (aka, an implementation detail); if not, it’s external.

**IDE tooling** will make it easy to spot external dependencies directly in your editor.

### Example

```kt
// Module: repositories
@Injectable
class UserRepository(
    dataSource: DataSource, // This dependency can’t be resolved inside this module, Sink treats it as external.
) {
    // repository implementation — doesn't care whether DataSource is MySQL/Postgres/etc.
}

fun main() {
    // Must pass DataSource explicitly because this module doesn't know its implementation
    val repo = InjectionCache().get<UserRepository>(object : DataSource { /* ... */ })
}
```

Contrast that with the application wiring module:

```kt
// Module: mySqlDataSource
@Injectable
class MySqlDataSource : DataSource {
    // MySQL-specific implementation
}

// Module: app (depends on repositories and mySqlDataSource)

// This module knows an injectable for DataSource! Otherwise, DataSource would've been an external dependency of App
@Injectable
class App(userRepository: UserRepository)

fun main() {
    // Here, DataSource is resolved automatically via MySqlDataSource
    val app = InjectionCache().get<App>()
}
```

A lot more to talk about, but so little time! I'll add more soon!

[//]: # (TODO: Add more)
package org.jetbrains.kotlin.compiler.plugin.template

public annotation class Injectable

public interface InjectionCache {
    // TODO: Investigate whether it's worth adding `InjectionCache` receiver, it might reduce the number of fields in the [compute] lambda object
    public fun <T> computeIfAbsent(key: Any?, compute: () -> T): T
}

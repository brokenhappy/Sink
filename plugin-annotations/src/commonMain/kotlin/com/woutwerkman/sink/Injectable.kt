package com.woutwerkman.sink

@Retention(AnnotationRetention.SOURCE)
public annotation class Injectable(
    /**
     * Can be "public", "private", or "internal".
     * By default, the visibility is inferred from the visibility of the return type
     */
    val visibility: String = "",
)

public abstract class InjectionCache {
    public companion object {
        public fun invoke(): InjectionCache = object : InjectionCache() { // TODO: Make thread safe!
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

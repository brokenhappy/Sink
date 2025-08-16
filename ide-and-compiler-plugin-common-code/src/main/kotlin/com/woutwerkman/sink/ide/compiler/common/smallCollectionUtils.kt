package com.woutwerkman.sink.ide.compiler.common

/** Semantically just [flatMap] But reduces allocations on the happy path, which is likely just a single item */
public inline fun <T, R> List<T>.flatMapLikely0Or1(mapper: (T) -> List<R>): List<R> {
    var resultSingle: List<R>? = null
    var resultMultiple: MutableList<R>? = null
    for (element in this) {
        val mapped = mapper(element)
        if (mapped.isEmpty()) continue
        if (resultMultiple != null) {
            resultMultiple.addAll(mapped)
        } else if (resultSingle != null) {
            resultMultiple = ArrayList(resultSingle.size + mapped.size)
            resultMultiple.addAll(resultSingle)
            resultMultiple.addAll(mapped)
        } else {
            resultSingle = mapped
        }
    }
    return resultMultiple ?: resultSingle ?: emptyList()
}

public inline fun <T, R> Collection<T>.mapLikely0Or1(mapper: (T) -> R): List<R> = when (size) {
    0 -> emptyList()
    1 -> listOf(mapper(first()))
    else -> mapTo(ArrayList(size), mapper)
}

/** Tries to reduce allocations for filter operations that are likely to zero or one item */
public inline fun <T> Iterable<T>.filterLikely0Or1(predicate: (T) -> Boolean): List<T> =
    mapNotNullLikely0Or1 { it.takeIf(predicate) }

/** Tries to reduce allocations for mapNotNull operations that are likely to zero or one item */
public inline fun <T, R : Any> Iterable<T>.mapNotNullLikely0Or1(predicate: (T) -> R?): List<R> {
    var singleResult: R? = null
    var multipleResult: MutableList<R>? = null
    for (element in this) {
        val element = predicate(element) ?: continue
        if (singleResult != null) {
            multipleResult = multipleResult ?: mutableListOf(singleResult)
            multipleResult.add(element)
        } else {
            singleResult = element
        }
    }
    return when {
        singleResult == null -> emptyList()
        multipleResult == null -> listOf(singleResult)
        else -> multipleResult
    }
}

public fun <T> List<T>.plusLikelyEmpty(other: List<T>): List<T> = when {
    other.isEmpty() -> this
    this.isEmpty() -> other
    else -> this + other
}
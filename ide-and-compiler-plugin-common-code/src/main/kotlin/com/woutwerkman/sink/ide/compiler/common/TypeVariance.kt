package com.woutwerkman.sink.ide.compiler.common

data class WithVariance<out T>(val variance: TypeVariance, val value: T)

enum class TypeVariance {
    /** Out. Is produced by functions */ Covariant,
    /** Is consumed and produced by functions */ Invariant,
    /** In. Is consumed by functions */ Contravariant,
}

fun TypeVariance.stepDown(next: TypeVariance): TypeVariance = when (this) {
    TypeVariance.Covariant -> next
    TypeVariance.Invariant -> TypeVariance.Invariant
    TypeVariance.Contravariant -> next.reverse()
}

fun TypeVariance.reverse(): TypeVariance = when (this) {
    TypeVariance.Contravariant -> TypeVariance.Covariant
    TypeVariance.Invariant -> TypeVariance.Invariant
    TypeVariance.Covariant -> TypeVariance.Contravariant
}
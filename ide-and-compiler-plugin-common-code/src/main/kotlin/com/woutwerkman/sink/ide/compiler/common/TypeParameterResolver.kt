package com.woutwerkman.sink.ide.compiler.common


class TypeParameterResolver<out TypeExpression, TypeParameterSymbol> private constructor(
    val typeMap: Map<TypeParameterSymbol, TypeExpression>,
    val parentResolver: TypeParameterResolver<TypeExpression, TypeParameterSymbol>? = null,
) {
    companion object Companion {
        private val Empty: TypeParameterResolver<Nothing, Any?> = TypeParameterResolver(emptyMap())
        fun <T> empty(): TypeParameterResolver<Nothing, T> = Empty as TypeParameterResolver<Nothing, T>

        operator fun <TypeExpression, TypeParameterSymbol> invoke(
            typeMap: Map<TypeParameterSymbol, TypeExpression>
        ): TypeParameterResolver<TypeExpression, TypeParameterSymbol> =
            if (typeMap.isEmpty()) empty() else TypeParameterResolver(typeMap, null)

        fun <TypeExpression, TypeParameterSymbol> TypeParameterResolver<TypeExpression, TypeParameterSymbol>.subResolver(
            child: Map<TypeParameterSymbol, TypeExpression>
        ): TypeParameterResolver<TypeExpression, TypeParameterSymbol> = when {
            child.isEmpty() -> this
            this == Empty -> TypeParameterResolver(typeMap = child)
            else -> TypeParameterResolver(typeMap = child, this)
        }
    }
}

open class Foo<in T>
class Bar<T>: Foo<T>()


fun <TypeExpression, TypeParameterSymbol> TypeParameterResolver<TypeExpression, TypeParameterSymbol>.resolve(
    symbol: TypeParameterSymbol,
): TypeExpression =
    typeMap[symbol] ?: parentResolver?.resolve(symbol) ?: error("Could not resolve: $symbol")

context(typeBehavior: TypeBehavior<TypeExpression, *, TypeParameterSymbol>)
fun <TypeExpression, TypeParameterSymbol> TypeParameterResolver<TypeExpression, TypeParameterSymbol>.resolveRecursive(
    expression: TypeExpression,
    variance: TypeVariance,
): WithVariance<TypeExpression> = typeBehavior.asTypeParameterReference(expression)?.let { parameterSymbol ->
    resolveRecursiveInternal(expression, variance)
        ?: WithVariance(variance.stepDown(typeBehavior.getVarianceOf(parameterSymbol)), resolve(parameterSymbol))
} ?: WithVariance(variance, expression)

context(typeBehavior: TypeBehavior<TypeExpression, *, TypeParameterSymbol>)
private fun <
    TypeExpression,
    TypeParameterSymbol,
    > TypeParameterResolver<TypeExpression, TypeParameterSymbol>.resolveRecursiveInternal(
    expression: TypeExpression,
    variance: TypeVariance,
): WithVariance<TypeExpression>? = typeBehavior.asTypeParameterReference(expression)?.let { parameterSymbol ->
    resolveRecursiveInternal(resolve(parameterSymbol), variance.stepDown(typeBehavior.getVarianceOf(parameterSymbol)))
}
package com.woutwerkman.sink.ide.compiler.common


public class TypeParameterResolver<out TypeExpression, TypeParameterSymbol> private constructor(
    public val typeMap: Map<TypeParameterSymbol, TypeExpression>,
    public val parentResolver: TypeParameterResolver<TypeExpression, TypeParameterSymbol>? = null,
) {
    public companion object Companion {
        private val Empty: TypeParameterResolver<Nothing, Any?> = TypeParameterResolver(emptyMap())
        public fun <T> empty(): TypeParameterResolver<Nothing, T> = Empty as TypeParameterResolver<Nothing, T>

        public operator fun <TypeExpression, TypeParameterSymbol> invoke(
            typeMap: Map<TypeParameterSymbol, TypeExpression>
        ): TypeParameterResolver<TypeExpression, TypeParameterSymbol> =
            if (typeMap.isEmpty()) empty() else TypeParameterResolver(typeMap, null)

        public fun <TypeExpression, TypeParameterSymbol> TypeParameterResolver<TypeExpression, TypeParameterSymbol>.subResolver(
            child: Map<TypeParameterSymbol, TypeExpression>
        ): TypeParameterResolver<TypeExpression, TypeParameterSymbol> = when {
            child.isEmpty() -> this
            this == Empty -> TypeParameterResolver(typeMap = child)
            else -> TypeParameterResolver(typeMap = child, this)
        }
    }
}

public open class Foo<in T>
public class Bar<T>: Foo<T>()


public fun <TypeExpression, TypeParameterSymbol> TypeParameterResolver<TypeExpression, TypeParameterSymbol>.resolve(
    symbol: TypeParameterSymbol,
): TypeExpression =
    typeMap[symbol] ?: parentResolver?.resolve(symbol) ?: error("Could not resolve: $symbol")

context(typeBehavior: TypeBehavior<TypeExpression, *, TypeParameterSymbol>)
public fun <TypeExpression, TypeParameterSymbol> TypeParameterResolver<TypeExpression, TypeParameterSymbol>.resolveRecursive(
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
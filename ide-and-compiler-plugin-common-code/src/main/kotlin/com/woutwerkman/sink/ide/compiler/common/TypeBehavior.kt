package com.woutwerkman.sink.ide.compiler.common

import com.woutwerkman.sink.ide.compiler.common.TypeParameterResolver.Companion.subResolver
import com.woutwerkman.sink.ide.compiler.common.TypeVariance.*


data class ConcreteType<TypeSymbol, TypeExpression>(val symbol: TypeSymbol, val genericArguments: List<TypeExpression>)

enum class DeclarationVisibility {
    Public,
    Internal,
    Protected,
    Private,
}

interface TypeBehavior<TypeExpression, TypeSymbol, TypeParameterSymbol> {
    fun getFqnOf(symbol: TypeSymbol): String
    fun getMinimumVisibilityOf(expression: TypeExpression): DeclarationVisibility
    fun isSubtype(subtype: TypeExpression, supertype: TypeExpression): Boolean =
        hasTypeRelation(subtype, TypeParameterResolver.empty(), supertype, TypeParameterResolver.empty(), Covariant)
    fun getVarianceOf(typeParameter: TypeParameterSymbol): TypeVariance
    fun asConcreteType(type: TypeExpression): ConcreteType<TypeSymbol, TypeExpression>?
    fun superTypesOfWithoutAny(symbol: TypeSymbol): Sequence<TypeExpression>
    fun getTypeParameterSymbolsOf(symbol: TypeSymbol): List<TypeParameterSymbol>
    fun isStarProjection(type: TypeExpression): Boolean
    fun unwrapNullableOrNull(type: TypeExpression): TypeExpression?
    /** AKA [Any]? */
    fun isTopType(type: TypeExpression): Boolean
    /** AKA [Nothing] */
    fun isBottomType(type: TypeExpression): Boolean
    fun asTypeParameterReference(type: TypeExpression): TypeParameterSymbol?
    fun unwrapVarianceOrNull(type: TypeExpression): WithVariance<TypeExpression>?
    fun areExactSameSymbol(lhs: TypeSymbol, rhs: TypeSymbol): Boolean = lhs == rhs
}

fun <
    TypeExpression,
    TypeSymbol,
    TypeParameterSymbol,
> TypeBehavior<TypeExpression, TypeSymbol, TypeParameterSymbol>.injectorFunctionNameOf(expression: TypeExpression): String =
    expression.whenn(
        isInOrOutType = { (variance, it) ->
            val varianceString = when (variance) {
                Covariant -> "Out"
                Invariant -> ""
                Contravariant -> "In"
            }
            "$varianceString${injectorFunctionNameOf(it)}"
        },
        isConcrete = { (symbol, parameters) ->
            getFqnOf(symbol).substringAfterLast(".").asValidJavaIdentifier() +
                if (parameters.isEmpty()) ""
                else parameters.joinToString(separator = "And", prefix = "Of", postfix = "") {
                    injectorFunctionNameOf(it)
                }
        },
        isStarProjection = { "Star" },
        isGeneric = { TODO("Support generics") },
        isNullable = { "${injectorFunctionNameOf(it)}OrNull" },
    )

fun <
    TypeExpression,
    TypeSymbol,
    TypeParameterSymbol,
> TypeBehavior<
    TypeExpression,
    TypeSymbol,
    TypeParameterSymbol,
>.hasTypeRelation(
    lhs: TypeExpression,
    lhsTypeParameterResolver: TypeParameterResolver<TypeExpression, TypeParameterSymbol> = TypeParameterResolver.empty(),
    rhs: TypeExpression,
    rhsTypeParameterResolver: TypeParameterResolver<TypeExpression, TypeParameterSymbol> = TypeParameterResolver.empty(),
    variance: TypeVariance,
): Boolean {
    assert(!isStarProjection(lhs)) { "Star projections should have been handled earlier. LHS: * RHS: $rhs" }
    assert(!isStarProjection(rhs)) { "Star projections should have been handled earlier. LHS: $lhs RHS: *" }
    if (variance == Contravariant)
        return hasTypeRelation(lhs = rhs, rhsTypeParameterResolver, rhs = lhs, lhsTypeParameterResolver, Covariant)

    fun recurse(
        newLhs: TypeExpression = lhs,
        newLhsTypeParameterResolver: TypeParameterResolver<TypeExpression, TypeParameterSymbol> = lhsTypeParameterResolver,
        newRhs: TypeExpression = rhs,
        newRhsTypeParameterResolver: TypeParameterResolver<TypeExpression, TypeParameterSymbol> = rhsTypeParameterResolver,
        newVariance: TypeVariance = variance,
    ): Boolean = hasTypeRelation(
        newLhs,
        newLhsTypeParameterResolver,
        newRhs,
        newRhsTypeParameterResolver,
        newVariance
    )

    // Handle in/out/generic
    unwrapVarianceOrNull(lhs)?.let { (lhsVariance, lhsSubexpression) ->
        return rhs.doesNotHaveDifferentVarianceFrom(lhsVariance) // Check for variance conflict first.
            && recurse(newLhs = lhsSubexpression, newVariance = variance.nextWhenLhsIs(lhsVariance))
    }
    asTypeParameterReference(lhs)?.let { genericParameter ->
        val lhsVariance = getVarianceOf(genericParameter)
        return rhs.doesNotHaveDifferentVarianceFrom(lhsVariance) // Check for variance conflict first.
            && recurse(
                newLhs = lhsTypeParameterResolver.resolve(genericParameter),
                newVariance = variance.nextWhenLhsIs(lhsVariance),
            )
    }

    unwrapVarianceOrNull(rhs)?.let { (rhsVariance, rhsSubexpression) ->
        // No need to handle variance conflicts here, they've been handled above.
        return recurse(newRhs = rhsSubexpression, newVariance = variance.nextWhenLhsIs(rhsVariance).reverse())
    }
    asTypeParameterReference(rhs)?.let { genericParameter ->
        // No need to handle variance conflicts here, they've been handled above.
        return recurse(
            newRhs = rhsTypeParameterResolver.resolve(genericParameter),
            newVariance = variance.nextWhenLhsIs(getVarianceOf(genericParameter)).reverse(),
        )
    }


    // Handle nullable types
    unwrapNullableOrNull(lhs)?.let { lhs ->
        return when (variance) {
            Covariant -> recurse(newLhs = lhs)
            Invariant -> unwrapNullableOrNull(rhs).isNotNullAnd { rhs -> recurse(newLhs = lhs, newRhs = rhs) }
            Contravariant -> error("Already handled")
        }
    }
    unwrapNullableOrNull(rhs)?.let { rhs ->
        return when (variance) {
            Covariant -> recurse(newRhs = rhs)
            Invariant -> false
            Contravariant -> error("Already handled")
        }
    }
    return when {
        isBottomType(lhs) -> isBottomType(rhs)
        isBottomType(rhs) -> false
        isTopType(lhs) -> isTopType(rhs) || variance == Covariant
        isTopType(rhs) -> false
        else -> hasConcreteTypeRelation(
            lhs,
            lhsTypeParameterResolver,
            rhs,
            rhsTypeParameterResolver,
            variance,
        )
    }
}

context(typeBehavior: TypeBehavior<TypeExpression, TypeSymbol, TypeParameterSymbol>)
private fun <
    TypeExpression,
    TypeParameterSymbol,
    TypeSymbol,
> TypeExpression.doesNotHaveDifferentVarianceFrom(otherVariance: TypeVariance): Boolean = (
    typeBehavior.unwrapVarianceOrNull(this)?.variance
        ?: typeBehavior.asTypeParameterReference(this)?.let { typeBehavior.getVarianceOf(it) }
).isNullOr { myVariance -> myVariance == otherVariance }

private fun <
    TypeExpression,
    TypeSymbol,
    TypeParameterSymbol,
> TypeBehavior<
    TypeExpression,
    TypeSymbol,
    TypeParameterSymbol,
>.hasConcreteTypeRelation(
    subtype: TypeExpression,
    subtypeParameterResolver: TypeParameterResolver<TypeExpression, TypeParameterSymbol>,
    supertype: TypeExpression,
    supertypeParameterResolver: TypeParameterResolver<TypeExpression, TypeParameterSymbol>,
    variance: TypeVariance,
): Boolean {
    val (subtypeSymbol, subtypeArguments) = asConcreteType(subtype) ?: return false
    val (supertypeSymbol, supertypeArguments) = asConcreteType(supertype) ?: return false
    val nextSubtypeResolver = subtypeParameterResolver.subResolver(subtypeSymbol, subtypeArguments)
    return if (!areExactSameSymbol(subtypeSymbol, supertypeSymbol)) {
        variance == Covariant && superTypesOfWithoutAny(subtypeSymbol).any { supertypeExpressionOfSubtype ->
            hasTypeRelation(
                supertypeExpressionOfSubtype,
                nextSubtypeResolver,
                supertype,
                supertypeParameterResolver,
                Covariant,
            )
        }
    } else zipAndAll(
        subtypeArguments,
        supertypeArguments,
        getTypeParameterSymbolsOf(subtypeSymbol)
    ) { lhsArgument, rhsArgument, parameter ->
        // This is some very very funky star projections behavior that has to be handled carefully...
        val newVariance = getVarianceOf(parameter)
        val (lhsVariance, lhs) = supertypeParameterResolver.resolveRecursive(lhsArgument, variance)
        val (rhsVariance, rhs) = supertypeParameterResolver.resolveRecursive(rhsArgument, variance)
        // TODO: LhsVariance and RhsVariance are unused, is that okay?
        assert(lhsVariance == rhsVariance) {
            // TODO: Actually possible: `Foo<in T>` and `Bar<T>: Foo<T>`. Make test
            "Hmmm, peculiar! How did $lhs get $lhsVariance as opposed to $rhs $rhsVariance?"
        }
        val lhsIsStarProjection = isStarProjection(lhs)
        val rhsIsStarProjection = isStarProjection(rhs)
        if (lhsIsStarProjection || rhsIsStarProjection) {
            if (lhsIsStarProjection && rhsIsStarProjection) true
            else if (variance == Invariant && newVariance == Invariant) false
            else if (lhsIsStarProjection) when (newVariance) {
                Covariant -> isTopType(rhs)
                Invariant -> false
                Contravariant -> isBottomType(rhs)
            }
            else variance == Covariant || when (newVariance) {
                Covariant -> isTopType(lhs)
                Invariant -> false
                Contravariant -> isBottomType(lhs)
            }
        } else hasTypeRelation( // Resume normally
            lhsArgument,
            nextSubtypeResolver,
            rhsArgument,
            supertypeParameterResolver.subResolver(supertypeSymbol, supertypeArguments),
            variance.stepDown(newVariance),
        )
    }
}

context(typeBehavior: TypeBehavior<TypeExpression, TypeSymbol, TypeParameterSymbol>)
private fun <
    TypeExpression,
    TypeSymbol,
    TypeParameterSymbol,
> TypeParameterResolver<
    TypeExpression,
    TypeParameterSymbol,
>.subResolver(
    type: TypeSymbol,
    arguments: List<TypeExpression>,
): TypeParameterResolver<TypeExpression, TypeParameterSymbol> = typeBehavior
    .getTypeParameterSymbolsOf(type)
    .ifEmpty { null }
    ?.let { parameters -> this.subResolver(parameters.zip(arguments).toMap()) }
    ?: this

/**
 * Imagine checking:
 * ```kt
 *   //          vv lhs variance (Contravariant)
 *   MutableList<in Int> <: MutableList<Number>
 *   //                  ^^ [this] variance (Covariant)
 * ```
 * Then we are looking for a subtype at top level so [this] == [Covariant].
 * But when we recurse down to check the type of the `MutableList` arguments, we need to know which variance to use.
 * So we'll be checking a [Covariant] relation of `in Int` to `Number`.
 * First we need to unwrap the `in` on the lhs.
 * That's when to use this function to determine the variance to check for `Int` to `Number`.
 */
private fun TypeVariance.nextWhenLhsIs(lhs: TypeVariance): TypeVariance = when (this) {
    Covariant -> lhs
    Invariant -> Invariant
    Contravariant -> lhs.reverse()
}

fun <T> T?.isNotNullAnd(predicate: (T) -> Boolean): Boolean = this != null && predicate(this)
fun <T> T?.isNullOr(predicate: (T) -> Boolean): Boolean = this == null || predicate(this)

/** The combination of [zip]ping 3 guaranteed same size lists and calling [all] */
private inline fun <T, U, V> zipAndAll(
    ts: List<T>,
    us: List<U>,
    vs: List<V>,
    predicate: (T, U, V) -> Boolean,
): Boolean = (0..< ts.size).all { predicate(ts[it], us[it], vs[it]) }

context(typeBehavior: TypeBehavior<TypeExpression, TypeSymbol, TypeParameterSymbol>)
inline fun <TypeExpression, TypeSymbol, TypeParameterSymbol, R> TypeExpression.whenn(
    isInOrOutType: (WithVariance<TypeExpression>) -> R,
    isConcrete: (ConcreteType<TypeSymbol, TypeExpression>) -> R,
    isStarProjection: () -> R,
    isGeneric: (genericIdentifier: TypeParameterSymbol) -> R,
    isNullable: (unwrapped: TypeExpression) -> R,
): R = typeBehavior.asConcreteType(this)?.let(isConcrete)
    ?: typeBehavior.unwrapVarianceOrNull(this)?.let(isInOrOutType)
    ?: (if (typeBehavior.isStarProjection(this)) isStarProjection() else null)
    ?: typeBehavior.asTypeParameterReference(this)?.let(isGeneric)
    ?: typeBehavior.unwrapNullableOrNull(this)?.let(isNullable)
    ?: error("Unexpected type expression: $this")

private fun String.asValidJavaIdentifier(): String {
    val first = if (this.first().isJavaIdentifierStart()) this.first() else '_'
    return first + this.drop(1).map { if (it.isJavaIdentifierPart()) it else '_' }.joinToString("")
}

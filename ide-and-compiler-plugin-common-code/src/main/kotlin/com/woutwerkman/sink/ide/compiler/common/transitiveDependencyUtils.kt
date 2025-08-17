package com.woutwerkman.sink.ide.compiler.common

import com.woutwerkman.sink.ide.compiler.common.DependencyGraphBuilder.ResolvedDependency
import com.woutwerkman.sink.ide.compiler.common.DependencyGraphBuilder.ResolvedDependency.ExternalDependency
import com.woutwerkman.sink.ide.compiler.common.DependencyGraphBuilder.ResolvedDependency.ImplementationDetail


context(typeBehavior: TypeBehavior<TypeExpression, TypeSymbol, *>)
fun <
    FunctionSymbol,
    TypeExpression,
    TypeSymbol,
    DeclarationContainer
> DependencyGraph<FunctionSymbol, TypeExpression, TypeSymbol, DeclarationContainer>.allExternalDependenciesOf(
    function: FunctionSymbol
): List<ExternalDependency<TypeExpression, FunctionSymbol, TypeSymbol, DeclarationContainer>> =
    this.injectables[function]!!.allExternalDependencies().let { dependencies ->
        val newDependencies = dependencies.toMutableList()
        var i = 0
        while (i < newDependencies.size) {
            val dependency = newDependencies[i]
            val dependencyTypeAlreadyExists = newDependencies.withIndex().any { (j, other) ->
                i != j && typeBehavior.isSubtype(other.type, dependency.type)
            }
            if(dependencyTypeAlreadyExists) newDependencies.removeAt(i)
            i++
        }
        newDependencies.sortBy { it.parameterName }
        val names = mutableSetOf<String>()
        newDependencies.mapInPlace { dependency ->
            if (names.add(dependency.parameterName)) dependency
            else generateSequence(0) { it + 1 }
                .map { dependency.parameterName + it }
                .first { names.add(it) }
                .let { dependency.copy(parameterName = it) }
        }
        newDependencies
    }

private fun <
    FunctionSymbol,
    TypeExpression,
    TypeSymbol,
    DeclarationContainer,
> List<ResolvedDependency<TypeExpression, FunctionSymbol, TypeSymbol, DeclarationContainer>>.allExternalDependencies(
): List<ExternalDependency<TypeExpression, FunctionSymbol, TypeSymbol, DeclarationContainer>> = flatMapLikely0Or1 { dependency ->
    when (dependency) {
        is ExternalDependency -> listOf(dependency)
        is ImplementationDetail -> dependency.transitiveDependencies.allExternalDependencies()
    }
}

private fun <T> MutableList<T>.mapInPlace(function: (T) -> T) {
    for (i in this.indices) {
        this[i] = function(this[i])
    }
}

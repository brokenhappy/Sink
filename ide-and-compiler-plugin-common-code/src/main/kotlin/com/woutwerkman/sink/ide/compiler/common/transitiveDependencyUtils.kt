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
            if(newDependencies.withIndex().any { (j, other) ->
                    i != j && typeBehavior.isSubtype(other.type, dependency.type)
                }) newDependencies.removeAt(i)
            i++
        }
        newDependencies.sortBy { it.parameterName }
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

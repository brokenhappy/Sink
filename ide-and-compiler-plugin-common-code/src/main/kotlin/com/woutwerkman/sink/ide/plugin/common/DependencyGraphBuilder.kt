package com.woutwerkman.sink.ide.plugin.common

import com.woutwerkman.sink.ide.plugin.common.DependencyGraphBuilder.ResolvedDependency
import java.util.*
import kotlin.collections.List
import kotlin.collections.associateWith
import kotlin.collections.emptyList


class DependencyGraphBuilder<TypeExpression, FunctionSymbol, TypeSymbol>(
    val functionBehavior: FunctionBehavior<TypeExpression, FunctionSymbol>,
    val typeBehavior: TypeBehavior<TypeExpression, TypeSymbol, *>,
) {
    sealed class ResolvedDependency<TypeExpression, FunctionSymbol> {
        data class MatchFound<TypeExpression, FunctionSymbol>(
            val parameterName: String,
            /**
             * In case of a function in our module's sources, this function is an [InstantiatorFunctionsDocRef].
             * In case of a function in a dependency module, this function is an [InjectorFunctionDocRef].
             */
            val instantiatorOrInjectorFunction: FunctionSymbol,
            val indirectDependencies: List<ResolvedDependency<TypeExpression, FunctionSymbol>>,
        ): ResolvedDependency<TypeExpression, FunctionSymbol>()
        data class MissingDependency<TypeExpression, FunctionSymbol>(
            val parameterName: String,
            val type: TypeExpression,
        ): ResolvedDependency<TypeExpression, FunctionSymbol>()
    }

    public fun buildGraph(
        injectablesOfThisModule: List<FunctionSymbol>,
        moduleDependencyGraphs: List<ModuleDependencyGraph<FunctionSymbol, TypeExpression, TypeSymbol>>,
    ): DependencyGraphFromSources<FunctionSymbol, TypeExpression, TypeSymbol> = context(functionBehavior, typeBehavior) {
        val superTypeMapOfThisModule = injectablesOfThisModule.asSupertypeMap()
        val module = injectablesOfThisModule
            .firstOrNull()
            ?.module
            ?: TODO("Handle")

        DependencyGraphFromSources(
            injectablesOfThisModule.associateWith { listOf() },
            superTypeMapOfThisModule,
            moduleDependencyGraphs = moduleDependencyGraphs,
        ).mapDependenciesRecursivelyMemoized { injectionFunction, _ ->
            injectionFunction.parameters.map { (name, type) ->
                val matches = superTypeMapOfThisModule
                    .findCandidatesThatProvide(type)
                    .plusLikelyEmpty(moduleDependencyGraphs.findInjectorsForType(type))

                if (matches.isEmpty()) ResolvedDependency.MissingDependency(name, type)
                else ResolvedDependency.MatchFound(
                    parameterName = name,
                    matches.single(),
                    recurse(matches.single()), // TODO: Handle ambiguous
                )
            }
        }
            .withIndirectMissingDependencies(module)
            .detectingCycles()
            .detectingDuplicates(moduleDependencyGraphs)
    }

    context(_: FunctionBehavior<TypeExpression, FunctionSymbol>)
    private fun List<FunctionSymbol>.asSupertypeMap(): Map<TypeSymbol, MutableList<FunctionSymbol>> =
        buildMap {
            this@asSupertypeMap.forEach { injectable ->
                typeBehavior
                    .asConcreteType(injectable.returnType)
                    ?.symbol
                    ?.also { symbol ->
                        getOrPut(symbol) { mutableListOf() } += injectable
                        this[symbol] = mutableListOf(injectable)
                    }
                    ?.let { symbol -> typeBehavior.superTypesOfWithoutAny(symbol) }
                    ?.forEach { superType ->
                        getOrPut(
                            typeBehavior.asConcreteType(superType)!!.symbol
                        ) { mutableListOf() } += injectable
                    }
            }
        }

    private val ResolvedDependency<*, *>.parameterName get(): String = when (this) {
        is ResolvedDependency.MatchFound -> parameterName
        is ResolvedDependency.MissingDependency -> parameterName
    }

    private fun ResolvedDependency<TypeExpression, FunctionSymbol>.withParameterName(newName: String): ResolvedDependency<TypeExpression, FunctionSymbol> = when (this) {
        is ResolvedDependency.MissingDependency -> copy(parameterName = newName)
        is ResolvedDependency.MatchFound -> copy(parameterName = newName)
    }

    context(functionBehavior: FunctionBehavior<TypeExpression, FunctionSymbol>)
    private fun DependencyGraphFromSources<FunctionSymbol, TypeExpression, TypeSymbol>.detectingCycles(
    ): DependencyGraphFromSources<FunctionSymbol, TypeExpression, TypeSymbol> =
        instantiatorFunctionsToDependencies
            .findCycles()
            .ifEmpty { null }
            ?.let { copy(cycles = it) }
            ?: this

    context(functionBehavior: FunctionBehavior<TypeExpression, FunctionSymbol>)
    private fun Map<FunctionSymbol, List<ResolvedDependency<TypeExpression, FunctionSymbol>>>.findCycles(): List<List<FunctionSymbol>> {
        val visited = mutableSetOf<FunctionSymbol>()
        val recursionStack = mutableSetOf<FunctionSymbol>()
        val cycles = mutableListOf<List<FunctionSymbol>>()
        val currentPath = LinkedList<FunctionSymbol>()

        fun dfs(node: FunctionSymbol) {
            if (node in recursionStack) {
                val cycleStart = currentPath.indexOf(node)
                cycles.add(currentPath.slice(cycleStart..< currentPath.size))
                return
            }
            if (node in visited) return

            visited.add(node)
            recursionStack.add(node)
            currentPath.addLast(node)

            this[node]?.forEach { next ->
                when (next) {
                    is ResolvedDependency.MatchFound -> next.instantiatorOrInjectorFunction.also(::dfs)
                    is ResolvedDependency.MissingDependency -> {}
                }
            }

            recursionStack.remove(node)
            currentPath.removeLast()
        }

        keys.forEach { node ->
            if (node !in visited) {
                dfs(node)
            }
        }

        return cycles
    }


    /**
     * // Module A
     *
     * fun foo(Baz, Bar): Foo
     *
     * // foo -> [
     * //   MissingDependency(Baz),
     * //   MissingDependency(Bar),
     * // ]
     * fun DependencyCache.Foo(Baz, Bar): Foo
     *
     * // Module B
     *
     * fun bar(Baz, Foobs): Bar
     *
     * // bar -> [
     * //   MissingDependency(Baz),
     * //   MissingDependency(Foobs),
     * // ]
     * fun DependencyCache.Bar(Baz, Foobs): Bar
     *
     * // Module C
     *
     * fun baz(Foobs): Baz
     *
     * fun bla(Foo): Bla
     *
     * // baz -> [
     * //   MissingDependency(Foobs),
     * // ]
     * // bla -> [
     * //   MatchFound(Baz, [MissingDependency(Foobs)])
     * //   MatchFound(Bar, [MatchFound(Baz, [MissingDependency(Foobs)]), MissingDependency(Foobs)])
     * // ]
     * fun DependencyCache.Bla(foobs: Foobs): Bla = bla(
     *   Foo(
     *     Baz(
     *       foobs,
     *     ),
     *     Bar(
     *       Baz(foobs),
     *       foobs,
     *     ),
     *   ),
     * )
     */
    context(
        functionBehavior: FunctionBehavior<TypeExpression, FunctionSymbol>,
        typeBehavior: TypeBehavior<TypeExpression, TypeSymbol, *>,
    )
    private fun DependencyGraphFromSources<FunctionSymbol, TypeExpression, TypeSymbol>.withIndirectMissingDependencies(
        module: Any?
    ): DependencyGraphFromSources<FunctionSymbol, TypeExpression, TypeSymbol> =
        mapDependenciesRecursivelyMemoized { injectable, dependencies ->
            // TODO: If we support more narrow scoped injectables. We should handle it here? Kinda similar to module border crossing
            val names = mutableSetOf<String>()
            fun List<ResolvedDependency<TypeExpression, FunctionSymbol>>.resolvingCrossModuleDependencies(
                moduleThatResolvedThisDependency: Any?,
            ): List<ResolvedDependency<TypeExpression, FunctionSymbol>> = mapNotNull { indirectDependency ->
                when (indirectDependency) {
                    is ResolvedDependency.MatchFound -> indirectDependency.copy(
                        indirectDependencies = recurse(indirectDependency.instantiatorOrInjectorFunction)
                            .resolvingCrossModuleDependencies(indirectDependency.instantiatorOrInjectorFunction.module),
                    )
                    is ResolvedDependency.MissingDependency -> {
                        if (moduleThatResolvedThisDependency != module) {
                            // The original declaration that depended on this could not resolve this dependency.
                            // However, we are a different module. We can try again!

                            // TODO: First: It might be that the user (perhaps unknowingly) added
                            // TODO: the indirect missing dependency to their own dependencies already (Removed the logic, needs verification)
                            findCandidateForType(indirectDependency.type)?.let { candidateFromThisModule ->
                                // Yay! We were able to resolve a dependency unlike the original module that declared it
                                // Okay, so far we know that:
                                //  - One of our dependencies was:
                                //    - From another module
                                //    - And had a dependency that it was not able to satisfy in
                                //      their own module (AKA, missing dependency)
                                //    - And we were able to satisfy this dependency in our own module
                                // The newly added dependency might again have its own missing dependencies.
                                // So we recurse down its missing dependencies as well.
                                ResolvedDependency.MatchFound(
                                    parameterName = indirectDependency.parameterName,
                                    instantiatorOrInjectorFunction = candidateFromThisModule,
                                    indirectDependencies = recurse(candidateFromThisModule)
                                        .resolvingCrossModuleDependencies(candidateFromThisModule.module),
                                )
                            } ?: indirectDependency
                        } else {
                            indirectDependency
                        }
                    }
                }
            }.map { dependency ->
                if (names.add(dependency.parameterName)) dependency
                else generateSequence(0) { it + 1 }
                    .map { dependency.parameterName + it }
                    .first { names.add(it) }
                    .let { dependency.withParameterName(it) }
            }

            dependencies.resolvingCrossModuleDependencies(injectable.module)
        }

    private fun Map<FunctionSymbol, List<ResolvedDependency<TypeExpression, FunctionSymbol>>>.mapDependenciesRecursivelyMemoized(
        mapper: (
            injectable: FunctionSymbol,
            oldDependencies: List<ResolvedDependency<TypeExpression, FunctionSymbol>>,
            recurse: (FunctionSymbol) -> List<ResolvedDependency<TypeExpression, FunctionSymbol>>,
        ) -> List<ResolvedDependency<TypeExpression, FunctionSymbol>>,
    ): Map<FunctionSymbol, List<ResolvedDependency<TypeExpression, FunctionSymbol>>> {
        val newMap = HashMap<FunctionSymbol, List<ResolvedDependency<TypeExpression, FunctionSymbol>>>(this.size)
        val visited = HashSet<FunctionSymbol>()
        fun recurse(injectable: FunctionSymbol): List<ResolvedDependency<TypeExpression, FunctionSymbol>> =
            newMap.computeIfAbsent(injectable) {
                val dependencies = this[injectable]!!
                if (!visited.add(injectable)) dependencies // Uh-oh! We encountered a cycle. In theory that might be introduced this mapping operation. We will just return the old values
                else mapper(injectable, dependencies, ::recurse)
            }

        return mapValues { (it, _) -> recurse(it) }
    }

    context(functionBehavior: FunctionBehavior<TypeExpression, FunctionSymbol>)
    private fun DependencyGraphFromSources<FunctionSymbol, TypeExpression, TypeSymbol>.detectingDuplicates(
        moduleDependencyGraphs: List<ModuleDependencyGraph<FunctionSymbol, TypeExpression, TypeSymbol>>,
    ): DependencyGraphFromSources<FunctionSymbol, TypeExpression, TypeSymbol> {
        val typesInThisModule = HashSet<TypeExpression>()
        val duplicateTypes = instantiatorFunctionsToDependencies
            .keys
            .asSequence()
            .map { it.returnType }
            .filterNotTo(HashSet(), typesInThisModule::add)

        for (moduleDependencyGraph in moduleDependencyGraphs) {
            moduleDependencyGraph
                .injectables
                .keys
                .asSequence()
                .map { it.returnType }
                .filterTo(duplicateTypes) { it in typesInThisModule }
        }

        return copy(duplicates = duplicateTypes.mapLikelyEmpty { type ->
            instantiatorFunctionsToDependencies.keys.filter { it.returnType == type }
        })
    }
}

/**
 * Instantiator functions are the user-defined functions annotated with @Injectable.
 * Instantiator functions are an implementation detail of a module,
 * and thus are not used in a module's dependency graph.
 */
private typealias InstantiatorFunctionsDocRef = Nothing

/** Injector functions are those generated as extension functions of DependencyCache. */
private typealias InjectorFunctionDocRef = Any

interface FunctionBehavior<TypeExpression, FunctionSymbol> {
    fun getReturnTypeOf(injectable: FunctionSymbol): TypeExpression
    fun getFqnOfInjectionFunctionOf(injectable: FunctionSymbol): String
    fun getParametersOf(injectable: FunctionSymbol): List<Pair<String, TypeExpression>>
    /** Only used as equatable to other results of [getModuleOf] */
    fun getModuleOf(injectable: FunctionSymbol): Any?
}

context(functionBehavior: FunctionBehavior<TypeExpression, FunctionSymbol>)
private val <TypeExpression, FunctionSymbol> FunctionSymbol.parameters: List<Pair<String, TypeExpression>> get() = functionBehavior.getParametersOf(this)

context(functionBehavior: FunctionBehavior<TypeExpression, FunctionSymbol>)
private val <TypeExpression, FunctionSymbol> FunctionSymbol.injectionFunctionFqn: String get() = functionBehavior.getFqnOfInjectionFunctionOf(this)

context(functionBehavior: FunctionBehavior<TypeExpression, FunctionSymbol>)
private val <TypeExpression, FunctionSymbol> FunctionSymbol.returnType: TypeExpression get() = functionBehavior.getReturnTypeOf(this)

context(functionBehavior: FunctionBehavior<*, FunctionSymbol>)
private val <FunctionSymbol> FunctionSymbol.module: Any? get() = functionBehavior.getModuleOf(this)

//fun <Type, TypeSymbol, FunctionSymbol> dependencyGraphFromSources(
//    sourceInjectables: List<FunctionSymbol>,
//): DependencyGraph<Type, TypeSymbol, FunctionSymbol> {
//    TODO()
//}

//class DependencyGraphImpl<Type, TypeSymbol, TypeParameterSymbol, FunctionSymbol>(
//    private val typeFqnToInjectionFunctionsMap: Map<String, List<String>>,
//    private val graph: Map<String, List<ResolvedDependency<Type>>>,
//    private val parentGraph: DependencyGraph<Type, TypeSymbol, TypeParameterSymbol, FunctionSymbol>? = null,
//): DependencyGraph<Type, TypeSymbol, TypeParameterSymbol, FunctionSymbol> {
//    context(_: TypeBehavior<Type, TypeSymbol, TypeParameterSymbol>, _: InjectableBehavior<Type, FunctionSymbol>)
//    override fun findCandidatesThatProvide(type: Type): List<FunctionSymbol> {
//        TODO("Not yet implemented")
//    }
////        typeFqnToInjectionFunctionsMap[type.fqnWithoutGenerics]
////            ?.map {  }
////            ?.filter { candidate -> type.isSubtypeOf(candidate.returnType) }
////            ?.plus(parentGraph?.findCandidatesThatProvide(type) ?: emptyList())
////            ?: emptyList()
//}

class DependencyGraphFromSources<FunctionSymbol, TypeExpression, TypeSymbol>(
    val instantiatorFunctionsToDependencies: Map<FunctionSymbol, List<ResolvedDependency<TypeExpression, FunctionSymbol>>>,
    val superTypesMap: Map<TypeSymbol, List<FunctionSymbol>>,
    val cycles: List<List<FunctionSymbol>> = emptyList(),
    val duplicates: List<List<FunctionSymbol>> = emptyList(),
    val moduleDependencyGraphs: List<ModuleDependencyGraph<FunctionSymbol, TypeExpression, TypeSymbol>>,
) {
    interface DependencyRecursionContext<FunctionSymbol, TypeExpression> {
        fun recurse(symbol: FunctionSymbol): List<ResolvedDependency<TypeExpression, FunctionSymbol>>
        fun findCandidateForType(type: TypeExpression): FunctionSymbol?
    }

    internal fun copy(
        injectables: Map<FunctionSymbol, List<ResolvedDependency<TypeExpression, FunctionSymbol>>> = this.instantiatorFunctionsToDependencies,
        superTypesMap: Map<TypeSymbol, List<FunctionSymbol>> = this.superTypesMap,
        cycles: List<List<FunctionSymbol>> = this.cycles,
        duplicates: List<List<FunctionSymbol>> = this.duplicates,
        moduleDependencyGraphs: List<ModuleDependencyGraph<FunctionSymbol, TypeExpression, TypeSymbol>> = this.moduleDependencyGraphs,
    ): DependencyGraphFromSources<FunctionSymbol, TypeExpression, TypeSymbol> =
        DependencyGraphFromSources(injectables, superTypesMap, cycles, duplicates, moduleDependencyGraphs)

    context(behavior: TypeBehavior<TypeExpression, TypeSymbol, *>, _: FunctionBehavior<TypeExpression, FunctionSymbol>)
    internal inline fun mapDependenciesRecursivelyMemoized(
        crossinline mapper: DependencyRecursionContext<FunctionSymbol, TypeExpression>.(
            injectable: FunctionSymbol,
            oldDependencies: List<ResolvedDependency<TypeExpression, FunctionSymbol>>,
        ) -> List<ResolvedDependency<TypeExpression, FunctionSymbol>>,
    ): DependencyGraphFromSources<FunctionSymbol, TypeExpression, TypeSymbol> {
        val newMap = HashMap<FunctionSymbol, List<ResolvedDependency<TypeExpression, FunctionSymbol>>>(
            instantiatorFunctionsToDependencies.size,
        )
        val visited = HashSet<FunctionSymbol>()

        val recursionContext = object : DependencyRecursionContext<FunctionSymbol, TypeExpression> {
            override fun recurse(symbol: FunctionSymbol): List<ResolvedDependency<TypeExpression, FunctionSymbol>> =
                newMap.getOrPut(symbol) {
                    val dependencies = instantiatorFunctionsToDependencies[symbol]!!
                    if (!visited.add(symbol)) emptyList() // Uh-oh! We encountered a cycle. We don't store the result in the map, but instead return an empty list.
                    else mapper(symbol, dependencies)
                }

            override fun findCandidateForType(type: TypeExpression): FunctionSymbol? =
                superTypesMap.findCandidatesThatProvide(type).singleOrNull() // TODO: Handle
        }

        return copy(
            injectables = instantiatorFunctionsToDependencies.mapValues { (it, _) -> recursionContext.recurse(it) },
        )
    }

}

private fun <T> List<T>.plusLikelyEmpty(other: List<T>): List<T> = when {
    other.isEmpty() -> this
    this.isEmpty() -> other
    else -> this + other
}

/** Semantically just [flatMap] But reduces allocations on the happy path, which is likely just a single item */
private inline fun <T, R> List<T>.flatMapLikelySingle(mapper: (T) -> List<R>): List<R> {
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

context(behavior: TypeBehavior<TypeExpression, TypeSymbol, *>, _: FunctionBehavior<TypeExpression, FunctionSymbol>)
internal fun <TypeExpression, FunctionSymbol, TypeSymbol> Map<TypeSymbol, List<FunctionSymbol>>.findCandidatesThatProvide(
    type: TypeExpression,
): List<FunctionSymbol> = behavior
    .asConcreteType(type)
    ?.symbol
    ?.let { symbol -> this[symbol] }
    ?.pickCandidatesToProvide(type)
    ?: emptyList()

context(functionBehavior: FunctionBehavior<TypeExpression, FunctionSymbol>, behavior: TypeBehavior<TypeExpression, TypeSymbol, *>)
private fun <TypeExpression, TypeSymbol, FunctionSymbol> Iterable<FunctionSymbol>.pickCandidatesToProvide(
    type: TypeExpression
): List<FunctionSymbol> {
    // Micro optimization to prevent unnecessary allocations for the common single or zero matches case
    var singleCandidate: FunctionSymbol? = null
    var multipleCandidates: MutableList<FunctionSymbol>? = null
    for (injectable in this) {
        val returnType = injectable.returnType
        if (!behavior.isSubtype(returnType, type)) continue
        if (singleCandidate != null) {
            multipleCandidates = multipleCandidates ?: mutableListOf(singleCandidate)
            multipleCandidates.add(injectable)
        } else {
            singleCandidate = injectable
        }
    }
    return when {
        singleCandidate == null -> emptyList() // Optimization path for zero matches
        multipleCandidates == null -> listOf(singleCandidate) // Optimization path for a single match
        else -> multipleCandidates // Ambiguous
    }
}

class ModuleDependencyGraph<FunctionSymbol, TypeExpression, TypeSymbol>(
    val injectables: Map<FunctionSymbol, List<ResolvedDependency.MissingDependency<TypeExpression, FunctionSymbol>>>,
    val superTypesMap: Map<TypeSymbol, List<FunctionSymbol>>,
) {
    context(typeBehavior: TypeBehavior<TypeExpression, TypeSymbol, *>, _: FunctionBehavior<TypeExpression, FunctionSymbol>)
    fun findCandidatesThatProvide(
        type: TypeExpression,
    ): List<FunctionSymbol> =
        typeBehavior
            .asConcreteType(type)
            ?.symbol
            ?.let { typeSymbol -> superTypesMap[typeSymbol]}
            ?.pickCandidatesToProvide(type)
            ?: emptyList()
}

private fun <T, R> Collection<T>.mapLikelyEmpty(mapper: (T) -> R): List<R> = when (size) {
    0 -> emptyList()
    1 -> listOf(mapper(first()))
    else -> mapTo(ArrayList(size), mapper)
}

context(behavior: TypeBehavior<TypeExpression, TypeSymbol, *>, _: FunctionBehavior<TypeExpression, FunctionSymbol>)
internal fun <
    FunctionSymbol,
    TypeExpression,
    TypeSymbol
> List<ModuleDependencyGraph<FunctionSymbol, TypeExpression, TypeSymbol>>.findInjectorsForType(
    type: TypeExpression,
): List<FunctionSymbol> =
    flatMapLikelySingle { it.findCandidatesThatProvide(type) }
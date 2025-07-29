package com.woutwerkman.sink.ide.plugin.common

import com.woutwerkman.sink.ide.plugin.common.DependencyGraphBuilder.ResolvedDependency
import java.util.*


class DependencyGraphBuilder<Type, Injectable>(
    val injectableBehavior: InjectableBehavior<Type, Injectable>,
) {
    sealed class BuildResult<Type, Injectable> {
        data class NoIssues<Type, Injectable>(
            val graph: Map<Injectable, List<ResolvedDependency<Type>>>,
        ): BuildResult<Type, Injectable>()
        data class CyclesFound<Type, Injectable>(
            val cycles: List<List<Injectable>>,
        ): BuildResult<Type, Injectable>()
        data class DuplicatesFound<Type, Injectable>(
            val duplicates: List<List<Injectable>>,
        ): BuildResult<Type, Injectable>()
    }

    internal fun BuildResult<Type, Injectable>.mapSuccess(
        transform: (BuildResult.NoIssues<Type, Injectable>) -> BuildResult<Type, Injectable>
    ): BuildResult<Type, Injectable> = when (this) {
        is BuildResult.CyclesFound<Type, Injectable>,
        is BuildResult.DuplicatesFound<Type, Injectable> -> this
        is BuildResult.NoIssues<Type, Injectable> -> transform(this)
    }

    sealed class ResolvedDependency<Type> {
        data class MatchFound<Type>(
            val parameterName: String,
            val injectedType: Type,
            val injectionFunctionFqn: String,
            val indirectDependencies: List<ResolvedDependency<Type>>,
        ): ResolvedDependency<Type>()
        data class MissingDependency<Type>(
            val parameterName: String,
            val type: Type,
        ): ResolvedDependency<Type>()
    }

    public fun buildGraph(
        injectablesOfThisModule: List<Injectable>,
        allAccessibleInjectables: List<Injectable>,
    ): BuildResult<Type, Injectable> = with(injectableBehavior) {
        injectablesOfThisModule
            .firstOrNull()
            ?.module
            ?.let { module -> buildGraphInternal(injectablesOfThisModule, allAccessibleInjectables, module) }
            ?: BuildResult.NoIssues(emptyMap())
    }
    private val ResolvedDependency<*>.parameterName get(): String = when (this) {
        is ResolvedDependency.MatchFound -> parameterName
        is ResolvedDependency.MissingDependency -> parameterName
    }

    private fun ResolvedDependency<Type>.withParameterName(newName: String): ResolvedDependency<Type> = when (this) {
        is ResolvedDependency.MissingDependency -> copy(parameterName = newName)
        is ResolvedDependency.MatchFound -> copy(parameterName = newName)
    }

    context(injectableBehavior: InjectableBehavior<Type, Injectable>)
    private fun buildGraphInternal(
        injectablesOfThisModule: List<Injectable>,
        allAccessibleInjectables: List<Injectable>,
        module: Any?
    ): BuildResult<Type, Injectable> = BuildResult.NoIssues(buildMap {
        injectablesOfThisModule.forEach { injectable ->
            this[injectable] = injectable.parameters.map { (parameterName, parameterType) ->
                allAccessibleInjectables.pickBestCandidateToProvide(parameterType)
                    ?.let {
                        ResolvedDependency.MatchFound(
                            parameterName = parameterName,
                            injectedType = it.returnType,
                            injectionFunctionFqn = it.injectionFunctionFqn,
                            indirectDependencies = emptyList(),
                        )
                    }
                    ?: ResolvedDependency.MissingDependency(parameterName, parameterType)
            }
        }
    })
        .detectingCycles()
        .withIndirectMissingDependencies(module)
        .detectingDuplicates(allAccessibleInjectables)


    private fun BuildResult<Type, Injectable>.detectingCycles(): BuildResult<Type, Injectable> = mapSuccess { graphWithoutIssues ->
        graphWithoutIssues.graph.findCycles().ifEmpty { null }?.let(BuildResult<Type, Injectable>::CyclesFound) ?: this
    }


    private fun Map<Injectable, List<ResolvedDependency<Type>>>.findCycles(): List<List<Injectable>> {
        val visited = mutableSetOf<Injectable>()
        val recursionStack = mutableSetOf<Injectable>()
        val cycles = mutableListOf<List<Injectable>>()
        val currentPath = LinkedList<Injectable>()

        fun dfs(node: Injectable) {
            if (node in recursionStack) {
                val cycleStart = currentPath.indexOf(node)
                cycles.add(currentPath.subList(cycleStart, currentPath.size))
                return
            }
            if (node in visited) return

            visited.add(node)
            recursionStack.add(node)
            currentPath.addLast(node)

            this[node]?.forEach { next ->
                when (next) {
                    is ResolvedDependency.MatchFound -> TODO() // next.returnType?.also(::dfs)
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
    context(injectableBehavior: InjectableBehavior<Type, Injectable>)
    private fun BuildResult<Type, Injectable>.withIndirectMissingDependencies(
        module: Any?
    ): BuildResult<Type, Injectable> = mapSuccess { graphWithoutIssues ->
        graphWithoutIssues.copy(graph = graphWithoutIssues.graph.mapDependenciesRecursivelyMemoized { injectable, dependencies, recurse ->
            // TODO: If we support more narrow scoped injectables. We should handle it here? Kinda similar to module border crossing
            val names = mutableSetOf<String>()
            fun List<ResolvedDependency<Type>>.resolvingCrossModuleDependencies(
                moduleThatResolvedThisDependency: Any?,
            ): List<ResolvedDependency<Type>> = mapNotNull { indirectDependency ->
                when (indirectDependency) {
                    is ResolvedDependency.MatchFound -> indirectDependency.copy(
                        indirectDependencies = TODO()
                        // recurse(indirectDependency.instantiatorFunction)
                        // .resolvingCrossModuleDependencies(indirectDependency.instantiatorFunction.module),
                    )
                    is ResolvedDependency.MissingDependency -> {
                        if (moduleThatResolvedThisDependency != module) {
                            // The original declaration that depended on this could not resolve this dependency.
                            // However, we are a different module. We can try again!

                            // TODO: First: It might be that the user (perhaps unknowingly) added
                            // TODO: the indirect missing dependency to their own dependencies already (Removed the logic, needs verification)
                            graphWithoutIssues
                                .graph
                                .keys
                                .pickBestCandidateToProvide(indirectDependency.type)
                                ?.let { candidateFromThisModule ->
                                    // Yay! We were able to resolve a dependency unlike the original module that declared it
                                    // Okay, so far we know that:
                                    //  - One of our dependencies was:
                                    //    - From another module
                                    //    - And had a dependency that it was not able to satisfy in
                                    //      their own module (AKA, missing dependency)
                                    //    - And we were able to satisfy this dependency in our own module
                                    // The newly added dependency might again have its own missing dependencies.
                                    // So we recurse down its missing dependencies as well.
                                    TODO()
//                                    ResolvedDependency.MatchFound(
//                                        parameterName = indirectDependency.parameterName,
//                                        instantiatorFunction = candidateFromThisModule,
//                                        indirectDependencies = recurse(candidateFromThisModule)
//                                            .resolvingCrossModuleDependencies(candidateFromThisModule.module),
//                                    )
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
        })
    }

    context(injectableBehavior: InjectableBehavior<Type, Injectable>)
    private fun Iterable<Injectable>.pickBestCandidateToProvide(type: Type): Injectable? =
        singleOrNull { it.returnType == type }


    private fun Map<Injectable, List<ResolvedDependency<Type>>>.mapDependenciesRecursivelyMemoized(
        mapper: (
            injectable: Injectable,
            oldDependencies: List<ResolvedDependency<Type>>,
            recurse: (Injectable) -> List<ResolvedDependency<Type>>,
        ) -> List<ResolvedDependency<Type>>,
    ): Map<Injectable, List<ResolvedDependency<Type>>> {
        val newMap = HashMap<Injectable, List<ResolvedDependency<Type>>>(this.size)
        val visited = HashSet<Injectable>()
        fun recurse(injectable: Injectable): List<ResolvedDependency<Type>> =
            newMap.computeIfAbsent(injectable) {
                val dependencies = this[injectable]!!
                if (!visited.add(injectable)) dependencies // Uh-oh! We encountered a cycle. In theory that might be introduced this mapping operation. We will just return the old values
                else mapper(injectable, dependencies, ::recurse)
            }

        return mapValues { (it, _) -> recurse(it) }
    }

    context(injectableBehavior: InjectableBehavior<Type, Injectable>)
    private fun BuildResult<Type, Injectable>.detectingDuplicates(
        allAccessibleInjectables: List<Injectable>,
    ): BuildResult<Type, Injectable> = mapSuccess { graphWithoutIssues ->
        allAccessibleInjectables
            .map { it.returnType }
            .filterNot(mutableListOf<Type>()::add)
            .intersect(graphWithoutIssues.graph.keys.mapTo(HashSet()) { it.returnType })
            .ifEmpty { null }
            ?.let { duplicateTypes ->
                BuildResult.DuplicatesFound(duplicateTypes.map { type ->
                    graphWithoutIssues.graph.keys.filter { it.returnType == type }
                })
            }
            ?: this
    }
}

interface InjectableBehavior<Type, Injectable> {
    fun getReturnTypeOf(injectable: Injectable): Type
    fun getFqnOfInjectionFunctionOf(injectable: Injectable): String
    fun getParametersOf(injectable: Injectable): List<Pair<String, Type>>
    /** Only used as equatable to other results of [getModuleOf] */
    fun getModuleOf(injectable: Injectable): Any?
}

context(injectableBehavior: InjectableBehavior<Type, Injectable>)
private val <Type, Injectable> Injectable.parameters: List<Pair<String, Type>> get() = injectableBehavior.getParametersOf(this)

context(injectableBehavior: InjectableBehavior<Type, Injectable>)
private val <Type, Injectable> Injectable.injectionFunctionFqn: String get() = injectableBehavior.getFqnOfInjectionFunctionOf(this)

context(injectableBehavior: InjectableBehavior<Type, Injectable>)
private val <Type, Injectable> Injectable.returnType: Type get() = injectableBehavior.getReturnTypeOf(this)

context(injectableBehavior: InjectableBehavior<*, Injectable>)
private val <Injectable> Injectable.module: Any? get() = injectableBehavior.getModuleOf(this)

interface DependencyGraph<TypeExpression, TypeSymbol, TypeParameterSymbol, Injectable> {
    context(_: TypeBehavior<TypeExpression, TypeSymbol, TypeParameterSymbol>, _: InjectableBehavior<TypeExpression, Injectable>)
    fun findCandidatesThatProvide(type: TypeExpression): List<Injectable>
}

//fun <Type, TypeSymbol, Injectable> dependencyGraphFromSources(
//    sourceInjectables: List<Injectable>,
//): DependencyGraph<Type, TypeSymbol, Injectable> {
//    TODO()
//}

class DependencyGraphImpl<Type, TypeSymbol, TypeParameterSymbol, Injectable>(
    private val typeFqnToInjectionFunctionsMap: Map<String, List<String>>,
    private val graph: Map<String, List<ResolvedDependency<Type>>>,
    private val parentGraph: DependencyGraph<Type, TypeSymbol, TypeParameterSymbol, Injectable>? = null,
): DependencyGraph<Type, TypeSymbol, TypeParameterSymbol, Injectable> {
    context(_: TypeBehavior<Type, TypeSymbol, TypeParameterSymbol>, _: InjectableBehavior<Type, Injectable>)
    override fun findCandidatesThatProvide(type: Type): List<Injectable> {
        TODO("Not yet implemented")
    }
//        typeFqnToInjectionFunctionsMap[type.fqnWithoutGenerics]
//            ?.map {  }
//            ?.filter { candidate -> type.isSubtypeOf(candidate.returnType) }
//            ?.plus(parentGraph?.findCandidatesThatProvide(type) ?: emptyList())
//            ?: emptyList()


}
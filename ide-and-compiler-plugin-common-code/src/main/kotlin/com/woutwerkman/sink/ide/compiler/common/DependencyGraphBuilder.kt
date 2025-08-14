package com.woutwerkman.sink.ide.compiler.common

import com.woutwerkman.sink.ide.compiler.common.DependencyGraphBuilder.ResolvedDependency
import com.woutwerkman.sink.ide.compiler.common.DependencyGraphBuilder.ResolvedDependency.ExternalDependency
import java.io.ByteArrayOutputStream
import java.util.*
import kotlin.collections.List
import kotlin.collections.emptyList


class DependencyGraphBuilder<TypeExpression, FunctionSymbol, TypeSymbol, DeclarationContainer>() {
    sealed class ResolvedDependency<TypeExpression, FunctionSymbol, TypeSymbol, DeclarationContainer> {
        abstract val graphWhereThisWasResolved: DependencyGraph<FunctionSymbol, TypeExpression, TypeSymbol, DeclarationContainer>
        data class ImplementationDetail<TypeExpression, FunctionSymbol, TypeSymbol, DeclarationContainer>(
            val parameterName: String,
            /**
             * In case of a function in our module's sources, this function is an [InstantiatorFunctionsDocRef].
             * In case of a function in a dependency module, this function is an [InjectorFunctionDocRef].
             */
            val instantiatorOrInjectorFunction: FunctionSymbol,
            val transitiveDependencies: List<ResolvedDependency<TypeExpression, FunctionSymbol, TypeSymbol, DeclarationContainer>>,
            override val graphWhereThisWasResolved: DependencyGraph<FunctionSymbol, TypeExpression, TypeSymbol, DeclarationContainer>,
        ): ResolvedDependency<TypeExpression, FunctionSymbol, TypeSymbol, DeclarationContainer>()
        data class ExternalDependency<TypeExpression, FunctionSymbol, TypeSymbol, DeclarationContainer>(
            val parameterName: String,
            val type: TypeExpression,
            override val graphWhereThisWasResolved: DependencyGraph<FunctionSymbol, TypeExpression, TypeSymbol, DeclarationContainer>,
        ): ResolvedDependency<TypeExpression, FunctionSymbol, TypeSymbol, DeclarationContainer>()
    }

    context(
        declarationContainerBehavior: DeclarationContainerBehavior<DeclarationContainer, FunctionSymbol>,
        functionBehavior: FunctionBehavior<TypeExpression, FunctionSymbol>,
        typeBehavior: TypeBehavior<TypeExpression, TypeSymbol, *>,
    )
    public fun buildGraph(
        container: DeclarationContainer,
        modulesDependencyGraph: ModulesDependencyGraph<FunctionSymbol, TypeExpression, TypeSymbol, DeclarationContainer>,
    ): DependencyGraphFromSources<FunctionSymbol, TypeExpression, TypeSymbol, DeclarationContainer> =
        hydratePublicInjectables(container, parentGraph = modulesDependencyGraph).also { graph ->
            fun hydrateDependenciesAndTryToResolveLocallyRecursive(
                graph: DependencyGraphFromSourcesImpl<FunctionSymbol, TypeExpression, TypeSymbol, DeclarationContainer>,
            ) {
                hydrateDependenciesAndTryToResolveLocally(graph)
                graph.children.forEach { child -> hydrateDependenciesAndTryToResolveLocallyRecursive(child) }
            }

            hydrateDependenciesAndTryToResolveLocallyRecursive(graph)
            graph.cycles = graph.instantiatorFunctionsToDependencies.findCycles()
        }

    context(
        declarationContainerBehavior: DeclarationContainerBehavior<DeclarationContainer, FunctionSymbol>,
        functionBehavior: FunctionBehavior<TypeExpression, FunctionSymbol>,
        typeBehavior: TypeBehavior<TypeExpression, TypeSymbol, *>,
    )
    internal fun hydratePublicInjectables(
        container: DeclarationContainer,
        parentGraph: DependencyGraph<FunctionSymbol, TypeExpression, TypeSymbol, DeclarationContainer>?,
    ): DependencyGraphFromSourcesImpl<FunctionSymbol, TypeExpression, TypeSymbol, DeclarationContainer> {
        fun DependencyGraphFromSourcesImpl<FunctionSymbol, TypeExpression, TypeSymbol, DeclarationContainer>.addFunction(
            instantiatorWithGraph: FunctionSymbolWithSourceGraph<TypeExpression, FunctionSymbol, TypeSymbol, DeclarationContainer>,
        ) {
            instantiatorFunctionsToDependencies[instantiatorWithGraph.value] = listOf()
            superTypesMap.addSupertypes(instantiatorWithGraph, instantiatorWithGraph.value.returnType)
        }

        fun DependencyGraphFromSourcesImpl<FunctionSymbol, TypeExpression, TypeSymbol, DeclarationContainer>.pushInjectableUpUntilMaximumVisibility(
            originatingContainerVisibility: DeclarationVisibility,
            instantiatorWithGraph: FunctionSymbolWithSourceGraph<TypeExpression, FunctionSymbol, TypeSymbol, DeclarationContainer>,
        ) {
            val parent = this.parent
            if (originatingContainerVisibility == DeclarationVisibility.Private || parent !is DependencyGraphFromSourcesImpl) {
                this.addFunction(instantiatorWithGraph)
            } else {
                parent.pushInjectableUpUntilMaximumVisibility(
                    originatingContainerVisibility = declarationContainerBehavior.getVisibilityOf(instantiatorWithGraph.graph.container),
                    instantiatorWithGraph
                )
            }
        }

        return DependencyGraphFromSourcesImpl(
            instantiatorFunctionsToDependencies = mutableMapOf(),
            superTypesMap = mutableMapOf(),
            container = container,
            parent = parentGraph,
        ).also { graph ->
            graph.children = declarationContainerBehavior
                .getChildrenOf(container)
                .map { declarationContainer ->
                    hydratePublicInjectables(declarationContainer, parentGraph = graph)
                }

            declarationContainerBehavior
                .getDeclarationsOf(container)
                .forEach { declaration ->
                    graph.pushInjectableUpUntilMaximumVisibility(
                        originatingContainerVisibility = functionBehavior.getVisibilityOf(declaration),
                        WithGraph(graph, declaration)
                    )
                }
        }
    }

    private fun ResolvedDependency<TypeExpression, FunctionSymbol, TypeSymbol, DeclarationContainer>.withParameterName(
        newName: String,
    ): ResolvedDependency<TypeExpression, FunctionSymbol, TypeSymbol, DeclarationContainer> = when (this) {
        is ExternalDependency -> copy(parameterName = newName)
        is ResolvedDependency.ImplementationDetail -> copy(parameterName = newName)
    }

    context(functionBehavior: FunctionBehavior<TypeExpression, FunctionSymbol>, typeBehavior: TypeBehavior<TypeExpression, TypeSymbol, *>)
    private fun hydrateDependenciesAndTryToResolveLocally(
        topLevelGraph: DependencyGraphFromSourcesImpl<FunctionSymbol, TypeExpression, TypeSymbol, DeclarationContainer>,
    ) {
        topLevelGraph.setDependenciesRecursivelyMemoized { graphThatHostsFunction, injectionFunction ->
            /**
             * // Module A
             *
             * fun foo(Baz, Bar): Foo
             *
             * // foo -> [
             * //   ExternalDependency(Baz),
             * //   ExternalDependency(Bar),
             * // ]
             * fun DependencyCache.Foo(Baz, Bar): Foo
             *
             * // Module B
             *
             * fun bar(Baz, Foobs): Bar
             *
             * // bar -> [
             * //   ExternalDependency(Baz),
             * //   ExternalDependency(Foobs),
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
             * //   ExternalDependency(Foobs),
             * // ]
             * // bla -> [
             * //   ImplementationDetail(Baz, [ExternalDependency(Foobs)])
             * //   ImplementationDetail(Bar, [ImplementationDetail(Baz, [ExternalDependency(Foobs)]), ExternalDependency(Foobs)])
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
            fun ResolvedDependency<TypeExpression, FunctionSymbol, TypeSymbol, DeclarationContainer>.resolvingCrossModuleDependencies(
            ): ResolvedDependency<TypeExpression, FunctionSymbol, TypeSymbol, DeclarationContainer> = when {
                graphWhereThisWasResolved === graphThatHostsFunction -> this
                this is ResolvedDependency.ImplementationDetail -> this.copy(
                    graphWhereThisWasResolved = graphThatHostsFunction,
                    transitiveDependencies =
                        recurse(this.graphWhereThisWasResolved, instantiatorOrInjectorFunction)
                            .mapNotNullLikely0Or1 { it as? ExternalDependency }
                            .mapLikely0Or1 { it.resolvingCrossModuleDependencies() }
                )
                this is ExternalDependency ->
                    // The original declaration that depended on this could not resolve this dependency.
                    // However, we are another graph. We can try again!
                    graphThatHostsFunction
                        .findCandidatesForType(this.type)
                        .singleOrNull() // TODO: Handle ambiguous
                        ?.let { (graphThatHostsThisCandidate, candidate) ->
                            // Yay! We were able to resolve a dependency unlike the graph where this dependency came from.
                            // Okay, so far we know that:
                            //  - One of our dependencies was:
                            //    - From another graph
                            //    - And had a dependency that it was not able to satisfy in
                            //      their own graph (AKA, external dependency)
                            //    - And we were able to satisfy this dependency in our own graph
                            // The newly added dependency might again have its own external dependencies.
                            // So we recurse down its external dependencies as well.
                            ResolvedDependency.ImplementationDetail(
                                parameterName = this.parameterName,
                                instantiatorOrInjectorFunction = candidate,
                                graphWhereThisWasResolved = graphThatHostsFunction,
                                transitiveDependencies = recurse(graphThatHostsThisCandidate, candidate)
                                    .mapNotNullLikely0Or1 { it as? ExternalDependency }
                                    .mapLikely0Or1 { it.resolvingCrossModuleDependencies() }
                            )
                        } ?: this
                else -> throw IllegalStateException("Unexpected dependency: $this")
            }

            injectionFunction.parameters.map { (name, type) ->
                val matches = graphThatHostsFunction.findCandidatesForType(type)
                val dependency =
                    if (matches.isEmpty()) ExternalDependency(name, type, graphThatHostsFunction)
                    else {
                        val (graph, match) = matches.single()
                        ResolvedDependency.ImplementationDetail(
                            parameterName = name,
                            instantiatorOrInjectorFunction = match,
                            transitiveDependencies = recurse(graph, match)
                                .mapNotNullLikely0Or1 { it as? ExternalDependency }
                                .mapLikely0Or1 { it.resolvingCrossModuleDependencies() },
                            graphWhereThisWasResolved = graphThatHostsFunction,
                        )
                    }
                dependency.resolvingCrossModuleDependencies()
            }
        }
    }

    private fun Map<FunctionSymbol, List<ResolvedDependency<TypeExpression, FunctionSymbol, TypeSymbol, DeclarationContainer>>>.findCycles(): List<List<FunctionSymbol>> {
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
                    is ResolvedDependency.ImplementationDetail -> next.instantiatorOrInjectorFunction.also(::dfs)
                    is ExternalDependency -> {}
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
}

internal val ResolvedDependency<*, *, *, *>.parameterName get(): String = when (this) {
    is ResolvedDependency.ImplementationDetail -> parameterName
    is ExternalDependency -> parameterName
}

internal typealias FunctionSymbolWithSourceGraph<TypeExpression, FunctionSymbol, TypeSymbol, DeclarationContainer> =
    WithGraph<
        TypeExpression,
        FunctionSymbol,
        TypeSymbol,
        DeclarationContainer,
        DependencyGraphFromSourcesImpl<FunctionSymbol, TypeExpression, TypeSymbol, DeclarationContainer>,
        FunctionSymbol,
    >

data class WithGraph<TypeExpression, FunctionSymbol, TypeSymbol, DeclarationContainer, out G: DependencyGraph<FunctionSymbol, TypeExpression, TypeSymbol, DeclarationContainer>, V>(
    val graph: G,
    val value: V,
)

context(_: FunctionBehavior<TypeExpression, FunctionSymbol>, typeBehavior: TypeBehavior<TypeExpression, TypeSymbol, *>)
private fun <
    FunctionSymbol,
    TypeExpression,
    TypeSymbol,
    T,
> MutableMap<TypeSymbol, MutableList<T>>.addSupertypes(
    injectableContainer: T,
    startType: TypeExpression,
) {
    typeBehavior
        .asConcreteType(startType)
        ?.symbol
        ?.also { symbol ->
            getOrPut(symbol) { mutableListOf() } += injectableContainer
        }
        ?.let { symbol -> typeBehavior.superTypesOfWithoutAny(symbol) }
        ?.forEach { superType ->
            getOrPut(typeBehavior.asConcreteType(superType)!!.symbol) { mutableListOf() } += injectableContainer
        }
}

sealed interface DependencyGraph<FunctionSymbol, TypeExpression, TypeSymbol, DeclarationContainer> {
//    val functionsToDependencies: Map<
//        FunctionSymbol,
//        List<WithGraph<
//            TypeExpression,
//            FunctionSymbol,
//            DependencyGraph<FunctionSymbol, TypeExpression, TypeSymbol, DeclarationContainer>,
//            ResolvedDependency<TypeExpression, FunctionSymbol, TypeSymbol, DeclarationContainer>,
//        >>
//    >
    context(typeBehavior: TypeBehavior<TypeExpression, TypeSymbol, *>, _: FunctionBehavior<TypeExpression, FunctionSymbol>)
    fun findCandidatesForType(type: TypeExpression): List<WithGraph<TypeExpression, FunctionSymbol, TypeSymbol, DeclarationContainer, DependencyGraph<FunctionSymbol, TypeExpression, TypeSymbol, DeclarationContainer>, FunctionSymbol>>
}

interface DependencyGraphFromSources<FunctionSymbol, TypeExpression, TypeSymbol, DeclarationContainer>: DependencyGraph<FunctionSymbol, TypeExpression, TypeSymbol, DeclarationContainer> {
    val superTypesMap: Map<TypeSymbol, List<WithGraph<TypeExpression, FunctionSymbol, TypeSymbol, DeclarationContainer, DependencyGraphFromSources<FunctionSymbol, TypeExpression, TypeSymbol, DeclarationContainer>, FunctionSymbol>>>
    val container: DeclarationContainer
    val cycles: List<List<FunctionSymbol>>
    val parent: DependencyGraph<FunctionSymbol, TypeExpression, TypeSymbol, DeclarationContainer>?
    val instantiatorFunctionsToDependencies: Map<FunctionSymbol, List<ResolvedDependency<TypeExpression, FunctionSymbol, TypeSymbol, DeclarationContainer>>>
    fun forEachInjectable(onInjectable: (FunctionSymbol) -> Unit)
    context(_: FunctionBehavior<TypeExpression, FunctionSymbol>, typeBehavior: TypeBehavior<TypeExpression, TypeSymbol, *>)
    fun serializeAsModuleDependencyGraph(): ByteArray
    val children: List<DependencyGraphFromSources<FunctionSymbol, TypeExpression, TypeSymbol, DeclarationContainer>>
}

internal class DependencyGraphFromSourcesImpl<FunctionSymbol, TypeExpression, TypeSymbol, DeclarationContainer>(
    override val instantiatorFunctionsToDependencies: MutableMap<FunctionSymbol, List<ResolvedDependency<TypeExpression, FunctionSymbol, TypeSymbol, DeclarationContainer>>>,
    override val superTypesMap: MutableMap<TypeSymbol, MutableList<FunctionSymbolWithSourceGraph<TypeExpression, FunctionSymbol, TypeSymbol, DeclarationContainer>>>,
    override var container: DeclarationContainer,
    override var cycles: List<List<FunctionSymbol>> = emptyList(),
    override var parent: DependencyGraph<FunctionSymbol, TypeExpression, TypeSymbol, DeclarationContainer>? = null,
) : DependencyGraphFromSources<FunctionSymbol, TypeExpression, TypeSymbol, DeclarationContainer> {
    override lateinit var children: List<DependencyGraphFromSourcesImpl<FunctionSymbol, TypeExpression, TypeSymbol, DeclarationContainer>>

    override fun forEachInjectable(onInjectable: (FunctionSymbol) -> Unit) {
        instantiatorFunctionsToDependencies.keys.forEach(onInjectable)
        children.forEach { it.forEachInjectable(onInjectable) }
    }

//    override val functionsToDependencies: Map<FunctionSymbol, List<ResolvedDependencyWithGraph<TypeExpression, FunctionSymbol>>>
//        get() = instantiatorFunctionsToDependencies

    context(_: TypeBehavior<TypeExpression, TypeSymbol, *>, _: FunctionBehavior<TypeExpression, FunctionSymbol>)
    override fun findCandidatesForType(type: TypeExpression): List<WithGraph<TypeExpression, FunctionSymbol, TypeSymbol, DeclarationContainer, DependencyGraph<FunctionSymbol, TypeExpression, TypeSymbol, DeclarationContainer>, FunctionSymbol>> =
        superTypesMap
            .findCandidatesThatProvide(type)
            .let { candidates -> parent?.findCandidatesForType(type)?.plusLikelyEmpty(candidates) ?: candidates }

    interface DependencyRecursionContext<FunctionSymbol, TypeExpression, TypeSymbol, DeclarationContainer> {
        fun recurse(
            graphThatHostsFunction: DependencyGraph<FunctionSymbol, TypeExpression, TypeSymbol, DeclarationContainer>,
            function: FunctionSymbol,
        ): List<ResolvedDependency<TypeExpression, FunctionSymbol, TypeSymbol, DeclarationContainer>>
    }

    context(_: FunctionBehavior<TypeExpression, FunctionSymbol>)
    internal inline fun setDependenciesRecursivelyMemoized(
        crossinline mapper: DependencyRecursionContext<FunctionSymbol, TypeExpression, TypeSymbol, DeclarationContainer>.(
            graphThatHostsFunction: DependencyGraphFromSourcesImpl<FunctionSymbol, TypeExpression, TypeSymbol, DeclarationContainer>,
            function: FunctionSymbol,
        ) -> List<ResolvedDependency<TypeExpression, FunctionSymbol, TypeSymbol, DeclarationContainer>>,
    ) {
        val cache = HashMap<FunctionSymbol, List<ResolvedDependency<TypeExpression, FunctionSymbol, TypeSymbol, DeclarationContainer>>>()
        val visited = HashSet<FunctionSymbol>()

        val recursionContext = object : DependencyRecursionContext<FunctionSymbol, TypeExpression, TypeSymbol, DeclarationContainer> {
            override fun recurse(
                graphThatHostsFunction: DependencyGraph<FunctionSymbol, TypeExpression, TypeSymbol, DeclarationContainer>,
                function: FunctionSymbol,
            ): List<ResolvedDependency<TypeExpression, FunctionSymbol, TypeSymbol, DeclarationContainer>> =
                when (graphThatHostsFunction) {
                    is DependencyGraphFromSources -> cache.getOrPut(function) {
                        if (!visited.add(function)) emptyList() // Uh-oh! We encountered a cycle. We don't store the result in the map, but instead return an empty list.
                        else mapper(graphThatHostsFunction as DependencyGraphFromSourcesImpl, function).also {
                            graphThatHostsFunction.instantiatorFunctionsToDependencies[function] = it
                        }
                    }
                    is ModulesDependencyGraph -> graphThatHostsFunction.injectables[function]!!
                }
        }

        instantiatorFunctionsToDependencies.keys.forEach { recursionContext.recurse(this, it) }
    }

    context(_: FunctionBehavior<TypeExpression, FunctionSymbol>, typeBehavior: TypeBehavior<TypeExpression, TypeSymbol, *>)
    override fun serializeAsModuleDependencyGraph(): ByteArray {
        val buffer = ByteArrayOutputStream(instantiatorFunctionsToDependencies.size * 32)
        buffer.writeInt(instantiatorFunctionsToDependencies.size)
        for (injectable in instantiatorFunctionsToDependencies.keys) {
            val fqnBytes =  (
                injectable
                    .fqn
                    .takeIf { '.' in it }
                    ?.substringBeforeLast(".")
                    ?.let { if (it.isEmpty()) "" else "$it." }
                    .let { it ?: "" } +
                    typeBehavior.injectorFunctionNameOf(injectable.returnType)
            ).encodeToByteArray()
            buffer.writeInt(fqnBytes.size)
            buffer.write(fqnBytes)
        }
        return buffer.toByteArray()
    }
}

context(_: FunctionBehavior<TypeExpression, FunctionSymbol>, _: TypeBehavior<TypeExpression, TypeSymbol, *>)
fun <FunctionSymbol, TypeExpression, TypeSymbol, DeclarationContainer> ModulesDependencyGraph<FunctionSymbol, TypeExpression, TypeSymbol, DeclarationContainer>.addFromBytes(
    injectorResolver: (fqnOfInjector: String) -> FunctionSymbol,
    bytes: ByteArray,
) {
    val size = bytes.readIntAt(0)
    var currentOffset = 4
    repeat(size) {
        val fqnSize = bytes.readIntAt(currentOffset).also { currentOffset += 4 }
        val fqn = bytes.decodeToString(currentOffset, currentOffset + fqnSize).also { currentOffset += fqnSize }
        val injector = injectorResolver(fqn)
        injectables[injector] = injector.parameters.map { (name, type) ->
            ExternalDependency(name, type, this)
        }
        supertypesMap.addSupertypes(injector, injector.returnType)
    }
}

class ModulesDependencyGraph<FunctionSymbol, TypeExpression, TypeSymbol, DeclarationContainer>(): DependencyGraph<FunctionSymbol, TypeExpression, TypeSymbol, DeclarationContainer> {
    val supertypesMap: MutableMap<TypeSymbol, MutableList<FunctionSymbol>> = mutableMapOf()
    val injectables: MutableMap<FunctionSymbol, List<ExternalDependency<TypeExpression, FunctionSymbol, TypeSymbol, DeclarationContainer>>> = mutableMapOf()

    context(typeBehavior: TypeBehavior<TypeExpression, TypeSymbol, *>, _: FunctionBehavior<TypeExpression, FunctionSymbol>)
    override fun findCandidatesForType(type: TypeExpression): List<WithGraph<TypeExpression, FunctionSymbol, TypeSymbol, DeclarationContainer, DependencyGraph<FunctionSymbol, TypeExpression, TypeSymbol, DeclarationContainer>, FunctionSymbol>> =
        typeBehavior
            .asConcreteType(type)
            ?.symbol
            ?.let { typeSymbol -> supertypesMap[typeSymbol]}
            ?.pickCandidatesToProvide(type, this)
            ?: emptyList()
}

private fun ByteArrayOutputStream.writeInt(size: Int) {
    write(size and 0xFF)
    write((size shr 8) and 0xFF)
    write((size shr 16) and 0xFF)
    write((size shr 24) and 0xFF)
}

private fun ByteArray.readIntAt(i: Int): Int =
    this[i].toInt() +
        this[i + 1].toInt().shr(8) +
        this[i + 2].toInt().shr(16) +
        this[i + 3].toInt().shr(24)

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
    fun getFqnOf(injectable: FunctionSymbol): String
    /**
     * If it's not an instantiator function, uses direct visibility.
     * Otherwise, it uses the explicit visibility from the annotation,
     * or it infers it from the visibility of the return type
     */
    fun getVisibilityOf(injectable: FunctionSymbol): DeclarationVisibility
    fun getParametersOf(injectable: FunctionSymbol): List<Pair<String, TypeExpression>>
}

context(functionBehavior: FunctionBehavior<TypeExpression, FunctionSymbol>)
private val <TypeExpression, FunctionSymbol> FunctionSymbol.parameters: List<Pair<String, TypeExpression>> get() = functionBehavior.getParametersOf(this)

context(functionBehavior: FunctionBehavior<TypeExpression, FunctionSymbol>)
private val <TypeExpression, FunctionSymbol> FunctionSymbol.fqn: String get() = functionBehavior.getFqnOf(this)

context(functionBehavior: FunctionBehavior<TypeExpression, FunctionSymbol>)
private val <TypeExpression, FunctionSymbol> FunctionSymbol.returnType: TypeExpression get() = functionBehavior.getReturnTypeOf(this)

private fun <T> List<T>.plusLikelyEmpty(other: List<T>): List<T> = when {
    other.isEmpty() -> this
    this.isEmpty() -> other
    else -> this + other
}

interface DeclarationContainerBehavior<DeclarationContainer, FunctionSymbol> {
    fun getChildrenOf(container: DeclarationContainer): List<DeclarationContainer>
    fun getDeclarationsOf(container: DeclarationContainer): List<FunctionSymbol>
    fun getVisibilityOf(container: DeclarationContainer): DeclarationVisibility
}

/** Semantically just [flatMap] But reduces allocations on the happy path, which is likely just a single item */
public inline fun <T, R> List<T>.flatMapLikelySingle(mapper: (T) -> List<R>): List<R> {
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
internal fun <TypeExpression, FunctionSymbol, TypeSymbol, DeclarationContainer, G : DependencyGraph<FunctionSymbol, TypeExpression, TypeSymbol, DeclarationContainer>> Map<TypeSymbol, List<WithGraph<TypeExpression, FunctionSymbol, TypeSymbol, DeclarationContainer, G, FunctionSymbol>>>.findCandidatesThatProvide(
    type: TypeExpression,
): List<WithGraph<TypeExpression, FunctionSymbol, TypeSymbol, DeclarationContainer, G, FunctionSymbol>> = behavior
    .asConcreteType(type)
    ?.symbol
    ?.let { symbol -> this[symbol] }
    ?.filterLikely0Or1 { injectableWithGraph -> behavior.isSubtype(injectableWithGraph.value.returnType, type) }
    ?: emptyList()

context(functionBehavior: FunctionBehavior<TypeExpression, FunctionSymbol>, behavior: TypeBehavior<TypeExpression, TypeSymbol, *>)
private fun <TypeExpression, TypeSymbol, FunctionSymbol, DeclarationContainer, G : DependencyGraph<FunctionSymbol, TypeExpression, TypeSymbol, DeclarationContainer>> Iterable<FunctionSymbol>.pickCandidatesToProvide(
    type: TypeExpression,
    graph: G,
): List<WithGraph<TypeExpression, FunctionSymbol, TypeSymbol, DeclarationContainer, G, FunctionSymbol>> = mapNotNullLikely0Or1 { injectable ->
    if (behavior.isSubtype(injectable.returnType, type)) WithGraph(graph, injectable) else null
}

private inline fun <T, R> Collection<T>.mapLikely0Or1(mapper: (T) -> R): List<R> = when (size) {
    0 -> emptyList()
    1 -> listOf(mapper(first()))
    else -> mapTo(ArrayList(size), mapper)
}

/** Tries to reduce allocations for filter operations that are likely to zero or one item */
private inline fun <T> Iterable<T>.filterLikely0Or1(predicate: (T) -> Boolean): List<T> =
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

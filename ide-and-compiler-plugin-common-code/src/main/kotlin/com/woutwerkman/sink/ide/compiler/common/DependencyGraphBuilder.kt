package com.woutwerkman.sink.ide.compiler.common

import com.woutwerkman.sink.ide.compiler.common.DependencyGraphBuilder.ResolvedDependency
import java.io.ByteArrayOutputStream
import java.util.*
import kotlin.collections.List
import kotlin.collections.associateWith
import kotlin.collections.emptyList

context(functionBehavior: FunctionBehavior<TypeExpression, FunctionSymbol>, typeBehavior: TypeBehavior<TypeExpression, TypeSymbol, *>)
fun <TypeExpression, FunctionSymbol, TypeSymbol> DependencyGraphBuilder(
): DependencyGraphBuilder<TypeExpression, FunctionSymbol, TypeSymbol> = DependencyGraphBuilder(functionBehavior, typeBehavior)

class DependencyGraphBuilder<TypeExpression, FunctionSymbol, TypeSymbol>(
    val functionBehavior: FunctionBehavior<TypeExpression, FunctionSymbol>,
    val typeBehavior: TypeBehavior<TypeExpression, TypeSymbol, *>,
) {
    sealed class ResolvedDependency<TypeExpression, FunctionSymbol> {
        data class ImplementationDetail<TypeExpression, FunctionSymbol>(
            val parameterName: String,
            /**
             * In case of a function in our module's sources, this function is an [InstantiatorFunctionsDocRef].
             * In case of a function in a dependency module, this function is an [InjectorFunctionDocRef].
             */
            val instantiatorOrInjectorFunction: FunctionSymbol,
            val indirectDependencies: List<ExternalDependency<TypeExpression, FunctionSymbol>>,
        ): ResolvedDependency<TypeExpression, FunctionSymbol>()
        data class ExternalDependency<TypeExpression, FunctionSymbol>(
            val parameterName: String,
            val type: TypeExpression,
        ): ResolvedDependency<TypeExpression, FunctionSymbol>()
    }

    public fun buildGraph(
        injectablesOfThisModule: List<FunctionSymbol>,
        moduleDependencyGraphs: List<ModuleDependencyGraph<FunctionSymbol, TypeExpression, TypeSymbol>>,
    ): DependencyGraphFromSources<FunctionSymbol, TypeExpression, TypeSymbol> = context(functionBehavior, typeBehavior) {
        val module = injectablesOfThisModule
            .firstOrNull()
            ?.module
            ?: return DependencyGraphFromSources.empty()

        DependencyGraphFromSources(
            instantiatorFunctionsToDependencies = injectablesOfThisModule.associateWith { listOf() },
            superTypesMap = injectablesOfThisModule.asSupertypeMap(),
            moduleDependencyGraphs = moduleDependencyGraphs,
        )
            .hydrateShallowDependencies()
            .withIndirectExternalDependencies(module)
            .detectingCycles()
            .detectingDuplicates(moduleDependencyGraphs)
    }

    context(_: FunctionBehavior<TypeExpression, FunctionSymbol>, _: TypeBehavior<TypeExpression, TypeSymbol, *>)
    private fun List<FunctionSymbol>.asSupertypeMap(): Map<TypeSymbol, MutableList<FunctionSymbol>> =
        buildMap {
            this@asSupertypeMap.forEach { injectable ->
                addSupertypesOf(injectable)
            }
        }

    private val ResolvedDependency<*, *>.parameterName get(): String = when (this) {
        is ResolvedDependency.ImplementationDetail -> parameterName
        is ResolvedDependency.ExternalDependency -> parameterName
    }

    private fun ResolvedDependency<TypeExpression, FunctionSymbol>.withParameterName(newName: String): ResolvedDependency<TypeExpression, FunctionSymbol> = when (this) {
        is ResolvedDependency.ExternalDependency -> copy(parameterName = newName)
        is ResolvedDependency.ImplementationDetail -> copy(parameterName = newName)
    }

    private fun DependencyGraphFromSources<FunctionSymbol, TypeExpression, TypeSymbol>.detectingCycles(
    ): DependencyGraphFromSources<FunctionSymbol, TypeExpression, TypeSymbol> =
        instantiatorFunctionsToDependencies
            .findCycles()
            .ifEmpty { null }
            ?.let { copy(cycles = it) }
            ?: this

    context(
        functionBehavior: FunctionBehavior<TypeExpression, FunctionSymbol>,
        typeBehavior: TypeBehavior<TypeExpression, TypeSymbol, *>,
    )
    private fun DependencyGraphFromSources<FunctionSymbol, TypeExpression, TypeSymbol>.hydrateShallowDependencies(
    ): DependencyGraphFromSources<FunctionSymbol, TypeExpression, TypeSymbol> =
        mapDependenciesRecursivelyMemoized { injectionFunction, _ ->
            injectionFunction.parameters.map { (name, type) ->
                val matches = findCandidatesForType(type)
                if (matches.isEmpty()) ResolvedDependency.ExternalDependency(name, type)
                else ResolvedDependency.ImplementationDetail(
                    parameterName = name,
                    matches.single(),
                    emptyList(),
                )
            }
    }

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
                    is ResolvedDependency.ImplementationDetail -> next.instantiatorOrInjectorFunction.also(::dfs)
                    is ResolvedDependency.ExternalDependency -> {}
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
    context(
        functionBehavior: FunctionBehavior<TypeExpression, FunctionSymbol>,
        typeBehavior: TypeBehavior<TypeExpression, TypeSymbol, *>,
    )
    private fun DependencyGraphFromSources<FunctionSymbol, TypeExpression, TypeSymbol>.withIndirectExternalDependencies(
        module: Any?
    ): DependencyGraphFromSources<FunctionSymbol, TypeExpression, TypeSymbol> =
        mapDependenciesRecursivelyMemoized { injectable, dependencies ->
            // TODO: If we support more narrow scoped injectables. We should handle it here? Kinda similar to module border crossing
            val names = mutableSetOf<String>()
            fun List<ResolvedDependency<TypeExpression, FunctionSymbol>>.resolvingCrossModuleDependencies(
                moduleThatResolvedThisDependency: Any?,
            ): List<ResolvedDependency<TypeExpression, FunctionSymbol>> = mapNotNull { indirectDependency ->
                when (indirectDependency) {
                    is ResolvedDependency.ImplementationDetail -> indirectDependency.copy(
                        indirectDependencies = recurse(indirectDependency.instantiatorOrInjectorFunction)
                            .resolvingCrossModuleDependencies(indirectDependency.instantiatorOrInjectorFunction.module)
                            .mapNotNull { it as? ResolvedDependency.ExternalDependency },
                    )
                    is ResolvedDependency.ExternalDependency -> {
                        if (moduleThatResolvedThisDependency != module) {
                            // The original declaration that depended on this could not resolve this dependency.
                            // However, we are a different module. We can try again!

                            // TODO: First: It might be that the user (perhaps unknowingly) added
                            // TODO: the indirect external dependency to their own dependencies already (Removed the logic, needs verification)
                            findCandidatesForType(indirectDependency.type)
                                .singleOrNull() // TODO: Handle ambiguous
                                ?.let { candidateFromThisModule ->
                                    // Yay! We were able to resolve a dependency unlike the original module that declared it
                                    // Okay, so far we know that:
                                    //  - One of our dependencies was:
                                    //    - From another module
                                    //    - And had a dependency that it was not able to satisfy in
                                    //      their own module (AKA, external dependency)
                                    //    - And we were able to satisfy this dependency in our own module
                                    // The newly added dependency might again have its own external dependencies.
                                    // So we recurse down its external dependencies as well.
                                    ResolvedDependency.ImplementationDetail(
                                        parameterName = indirectDependency.parameterName,
                                        instantiatorOrInjectorFunction = candidateFromThisModule,
                                        indirectDependencies = recurse(candidateFromThisModule)
                                            .resolvingCrossModuleDependencies(candidateFromThisModule.module)
                                            .mapNotNull { it as? ResolvedDependency.ExternalDependency },
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

context(_: FunctionBehavior<TypeExpression, FunctionSymbol>, typeBehavior: TypeBehavior<TypeExpression, TypeSymbol, *>)
private fun <
    FunctionSymbol,
    TypeExpression,
    TypeSymbol
> MutableMap<TypeSymbol, MutableList<FunctionSymbol>>.addSupertypesOf(injectable: FunctionSymbol) {
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

data class DependencyGraphFromSources<FunctionSymbol, TypeExpression, TypeSymbol>(
    val instantiatorFunctionsToDependencies: Map<FunctionSymbol, List<ResolvedDependency<TypeExpression, FunctionSymbol>>>,
    val superTypesMap: Map<TypeSymbol, List<FunctionSymbol>>,
    val cycles: List<List<FunctionSymbol>> = emptyList(),
    val duplicates: List<List<FunctionSymbol>> = emptyList(),
    val moduleDependencyGraphs: List<ModuleDependencyGraph<FunctionSymbol, TypeExpression, TypeSymbol>>,
) {
    companion object {
        private val Empty = DependencyGraphFromSources<Nothing, Nothing, Nothing>(
            emptyMap(), emptyMap(), emptyList(), emptyList(), emptyList(),
        )

        @Suppress("UNCHECKED_CAST")
        fun <FunctionSymbol, TypeExpression, TypeSymbol> empty() =
            Empty as DependencyGraphFromSources<FunctionSymbol, TypeExpression, TypeSymbol>
    }

    context(behavior: TypeBehavior<TypeExpression, TypeSymbol, *>, _: FunctionBehavior<TypeExpression, FunctionSymbol>)
    fun findCandidatesForType(type: TypeExpression): List<FunctionSymbol> =
        superTypesMap
            .findCandidatesThatProvide(type)
            .plusLikelyEmpty(moduleDependencyGraphs.flatMapLikelySingle { it.findCandidatesThatProvide(type) })

    interface DependencyRecursionContext<FunctionSymbol, TypeExpression> {
        fun recurse(symbol: FunctionSymbol): List<ResolvedDependency<TypeExpression, FunctionSymbol>>
    }

    context(_: FunctionBehavior<TypeExpression, FunctionSymbol>)
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
                    val dependencies = instantiatorFunctionsToDependencies[symbol] ?: moduleDependencyGraphs
                        .firstNotNullOf { it.injectables[symbol] }
                    if (!visited.add(symbol)) emptyList() // Uh-oh! We encountered a cycle. We don't store the result in the map, but instead return an empty list.
                    else mapper(symbol, dependencies)
                }
        }

        return copy(
            instantiatorFunctionsToDependencies =
                instantiatorFunctionsToDependencies.mapValues { (it, _) -> recursionContext.recurse(it) },
        )
    }

    context(_: FunctionBehavior<TypeExpression, FunctionSymbol>, typeBehavior: TypeBehavior<TypeExpression, TypeSymbol, *>)
    fun serializeAsModuleDependencyGraph(): ByteArray {
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
fun <FunctionSymbol, TypeExpression, TypeSymbol> moduleDependencyGraphFromBytes(
    injectorResolver: (fqnOfInjector: String) -> FunctionSymbol,
    bytes: ByteArray,
): ModuleDependencyGraph<FunctionSymbol, TypeExpression, TypeSymbol> {
    val size = bytes.readIntAt(0)
    var currentOffset = 4
    val supertypeMap = mutableMapOf<TypeSymbol, MutableList<FunctionSymbol>>()
    val injectables = buildMap {
        repeat(size) {
            val fqnSize = bytes.readIntAt(currentOffset).also { currentOffset += 4 }
            val fqn = bytes.decodeToString(currentOffset, currentOffset + fqnSize).also { currentOffset += fqnSize }
            val injector = injectorResolver(fqn)
            this[injector] = injector.parameters.map { (name, type) ->
                ResolvedDependency.ExternalDependency<TypeExpression, FunctionSymbol>(name, type)
            }
            supertypeMap.addSupertypesOf(injector)
        }
    }
    return ModuleDependencyGraph(injectables, supertypeMap)
}

class ModuleDependencyGraph<FunctionSymbol, TypeExpression, TypeSymbol>(
    val injectables: Map<FunctionSymbol, List<ResolvedDependency.ExternalDependency<TypeExpression, FunctionSymbol>>>,
    val superTypesMap: Map<TypeSymbol, List<FunctionSymbol>>,
) {
    context(typeBehavior: TypeBehavior<TypeExpression, TypeSymbol, *>, _: FunctionBehavior<TypeExpression, FunctionSymbol>)
    fun findCandidatesThatProvide(type: TypeExpression): List<FunctionSymbol> =
        typeBehavior
            .asConcreteType(type)
            ?.symbol
            ?.let { typeSymbol -> superTypesMap[typeSymbol]}
            ?.pickCandidatesToProvide(type)
            ?: emptyList()
}

private fun ByteArrayOutputStream.writeInt(size: Int) {
    write(size and 0xFF)
    write((size shr 8) and 0xFF)
    write((size shr 16) and 0xFF)
    write((size shr 24) and 0xFF)
}

private fun <TypeExpression, FunctionSymbol> List<ResolvedDependency<TypeExpression, FunctionSymbol>>.forEachExternalDependencyRecursive(
    function: (ResolvedDependency.ExternalDependency<TypeExpression, FunctionSymbol>) -> Unit,
) {
    forEach { dependency ->
        when (dependency) {
            is ResolvedDependency.ImplementationDetail -> dependency.indirectDependencies.forEachExternalDependencyRecursive(function)
            is ResolvedDependency.ExternalDependency -> function(dependency)
        }
    }
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
    fun getParametersOf(injectable: FunctionSymbol): List<Pair<String, TypeExpression>>
    /** Only used as equatable to other results of [getModuleOf] */
    fun getModuleOf(injectable: FunctionSymbol): Any?
}

context(functionBehavior: FunctionBehavior<TypeExpression, FunctionSymbol>)
private val <TypeExpression, FunctionSymbol> FunctionSymbol.parameters: List<Pair<String, TypeExpression>> get() = functionBehavior.getParametersOf(this)

context(functionBehavior: FunctionBehavior<TypeExpression, FunctionSymbol>)
private val <TypeExpression, FunctionSymbol> FunctionSymbol.fqn: String get() = functionBehavior.getFqnOf(this)

context(functionBehavior: FunctionBehavior<TypeExpression, FunctionSymbol>)
private val <TypeExpression, FunctionSymbol> FunctionSymbol.returnType: TypeExpression get() = functionBehavior.getReturnTypeOf(this)

context(functionBehavior: FunctionBehavior<*, FunctionSymbol>)
private val <FunctionSymbol> FunctionSymbol.module: Any? get() = functionBehavior.getModuleOf(this)

private fun <T> List<T>.plusLikelyEmpty(other: List<T>): List<T> = when {
    other.isEmpty() -> this
    this.isEmpty() -> other
    else -> this + other
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
): List<FunctionSymbol> = filterLikelySingle { injectable -> behavior.isSubtype(injectable.returnType, type) }

private inline fun <T, R> Collection<T>.mapLikelyEmpty(mapper: (T) -> R): List<R> = when (size) {
    0 -> emptyList()
    1 -> listOf(mapper(first()))
    else -> mapTo(ArrayList(size), mapper)
}

/** Tries to reduce allocations for filter operations that are likely to zero or one item */
private inline fun <T> Iterable<T>.filterLikelySingle(predicate: (T) -> Boolean): List<T> {
    var singleResult: T? = null
    var multipleResult: MutableList<T>? = null
    for (element in this) {
        if (!predicate(element)) continue
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

package org.jetbrains.kotlin.compiler.plugin.template.ir

// TODO: Ensure Nullability handled
// TODO: Add support for kx Serializer injection? Might be cool icw generic support

import org.jetbrains.kotlin.backend.common.extensions.IrGenerationExtension
import org.jetbrains.kotlin.backend.common.extensions.IrPluginContext
import org.jetbrains.kotlin.backend.common.getCompilerMessageLocation
import org.jetbrains.kotlin.backend.jvm.ir.isInCurrentModule
import org.jetbrains.kotlin.cli.common.messages.CompilerMessageSeverity
import org.jetbrains.kotlin.cli.common.messages.MessageCollector
import org.jetbrains.kotlin.descriptors.DescriptorVisibilities
import org.jetbrains.kotlin.descriptors.Modality
import org.jetbrains.kotlin.descriptors.ModuleDescriptor
import org.jetbrains.kotlin.descriptors.impl.PackageFragmentDescriptorImpl
import org.jetbrains.kotlin.fir.FirModuleData
import org.jetbrains.kotlin.fir.descriptors.FirModuleDescriptor
import org.jetbrains.kotlin.ir.IrElement
import org.jetbrains.kotlin.ir.declarations.IrConstructor
import org.jetbrains.kotlin.ir.declarations.IrDeclaration
import org.jetbrains.kotlin.ir.declarations.IrDeclarationOrigin
import org.jetbrains.kotlin.ir.declarations.IrFunction
import org.jetbrains.kotlin.ir.declarations.IrModuleFragment
import org.jetbrains.kotlin.ir.declarations.IrSimpleFunction
import org.jetbrains.kotlin.ir.declarations.impl.IrFileImpl
import org.jetbrains.kotlin.ir.declarations.impl.IrFunctionImpl
import org.jetbrains.kotlin.ir.expressions.IrConstantPrimitive
import org.jetbrains.kotlin.ir.expressions.impl.IrConstImpl
import org.jetbrains.kotlin.ir.expressions.impl.IrConstructorCallImpl
import org.jetbrains.kotlin.ir.symbols.impl.IrFileSymbolImpl
import org.jetbrains.kotlin.ir.symbols.impl.IrSimpleFunctionSymbolImpl
import org.jetbrains.kotlin.ir.types.IrType
import org.jetbrains.kotlin.ir.types.typeWith
import org.jetbrains.kotlin.ir.util.NaiveSourceBasedFileEntryImpl
import org.jetbrains.kotlin.ir.util.SYNTHETIC_OFFSET
import org.jetbrains.kotlin.ir.util.addChild
import org.jetbrains.kotlin.ir.util.addFile
import org.jetbrains.kotlin.ir.util.callableId
import org.jetbrains.kotlin.ir.util.constructors
import org.jetbrains.kotlin.ir.util.file
import org.jetbrains.kotlin.ir.util.fileOrNull
import org.jetbrains.kotlin.ir.util.functions
import org.jetbrains.kotlin.ir.util.hasAnnotation
import org.jetbrains.kotlin.ir.util.parentAsClass
import org.jetbrains.kotlin.ir.util.render
import org.jetbrains.kotlin.ir.visitors.IrVisitorVoid
import org.jetbrains.kotlin.ir.visitors.acceptChildrenVoid
import org.jetbrains.kotlin.js.parser.sourcemaps.JsonArray
import org.jetbrains.kotlin.js.parser.sourcemaps.JsonObject
import org.jetbrains.kotlin.name.CallableId
import org.jetbrains.kotlin.name.ClassId
import org.jetbrains.kotlin.name.FqName
import org.jetbrains.kotlin.name.Name
import org.jetbrains.kotlin.resolve.scopes.MemberScope

class SimpleIrGenerationExtension: IrGenerationExtension {
    override fun generate(moduleFragment: IrModuleFragment, pluginContext: IrPluginContext) {
        // TODO: Use explicit-API compiler flag for some things?
        val injectableCacheInterface = pluginContext.referenceClass(ClassId.topLevel(injectionCacheFqn))
            ?: return // Early exit, this module does not include Sink compile time dependency
        val injectablesDeclaredInThisModule = collectInjectableImplementationsOf(moduleFragment)
        if (injectablesDeclaredInThisModule.isEmpty()) return
        val injectableCacheType by lazy { injectableCacheInterface.typeWith() }

        val injectablesDeclaredInDependingModules = pluginContext
            .referenceAllInjectablesDeclaredInDependencyModulesOf(moduleFragment)

        val accessibleInjectables = injectablesDeclaredInDependingModules + injectablesDeclaredInThisModule
        val graphWithoutIssues = when (
            val result = injectablesDeclaredInThisModule.buildGraph(accessibleInjectables, moduleFragment)
        ) {
            is GraphBuildResult.CyclesFound -> {
                result.cycles.forEach { cycle ->
                    pluginContext.messageCollector.reportErrorsForCycle(cycle)
                }
                return // TODO: Don't return but continue with throwing stubs. Being more permissive might reveal more errors.
            }
            is GraphBuildResult.DuplicatesFound -> {
                result.duplicates.forEach { duplicates ->
                    pluginContext.messageCollector.reportErrorsForDuplicates(duplicates)
                }
                return // TODO: Don't return but continue with throwing stubs. Being more permissive might reveal more errors.
            }
            is GraphBuildResult.NoIssues -> result
        }

        val creationSession = InjectionFunctionCreationSession(
            pluginContext,
            moduleFragment,
            injectableCacheType,
            injectionCacheComputeIfAbsentMethodSymbol = injectableCacheInterface.functions.first().owner.symbol,
        )

        data class InjectableAndInjectionFunction(val injectable: IrFunction, val injectionFunction: IrSimpleFunction)

        val injectablesAndTheirInjectionFunctions = graphWithoutIssues
            .graph
            .keys
            .map { injectable ->
                InjectableAndInjectionFunction(
                    injectable = injectable,
                    injectionFunction = creationSession.generateInjectionFunction(injectable, graphWithoutIssues),
                )
            }

        // Now persist the generated declarations
        injectablesAndTheirInjectionFunctions.forEach { (injectable, injectionFunction) ->
            injectable.file.addChild(injectionFunction)
        }

        val callableId = moduleFragment.descriptor.stableOrRegularName().getModuleMetadataFunctionId()
        moduleFragment.addFile(IrFileImpl(
            fileEntry = NaiveSourceBasedFileEntryImpl(
                name = "__File_Name_That_A_Plugin_Must_Provide_For_No_Apparent_Reason__",
            ),
            symbol = IrFileSymbolImpl(
                packageFragmentWithOnlyASingleFunction(moduleFragment.descriptor, callableId)
            ),
            fqName = callableId.packageName,
            module = moduleFragment,
        ).also { file ->
            file.addChild(pluginContext.irFactory.createSimpleFunction(
                startOffset = SYNTHETIC_OFFSET,
                endOffset = SYNTHETIC_OFFSET,
                origin = IrDeclarationOrigin.GeneratedByPlugin(SinkPluginKey),
                name = callableId.callableName,
                visibility = DescriptorVisibilities.PUBLIC,
                isInline = false,
                isExpect = false,
                returnType = pluginContext.irBuiltIns.unitType,
                modality = Modality.FINAL,
                symbol = IrSimpleFunctionSymbolImpl(),
                isTailrec = false,
                isSuspend = false,
                isOperator = false,
                isInfix = false,
            ).also { function ->
                val metadataAnnotationClass = pluginContext
                    .referenceClass(ClassId.topLevel(metadataAnnotationFqn)) ?: TODO()
                (function as IrFunctionImpl).annotations += IrConstructorCallImpl(
                    startOffset = SYNTHETIC_OFFSET,
                    endOffset = SYNTHETIC_OFFSET,
                    type = metadataAnnotationClass.typeWith(),
                    symbol = metadataAnnotationClass.constructors.first(),
                    typeArgumentsCount = 0,
                    constructorTypeArgumentsCount = 0,
                ).also { annotation ->
                    annotation.arguments[0] = IrConstImpl.string(
                        startOffset = SYNTHETIC_OFFSET,
                        endOffset = SYNTHETIC_OFFSET,
                        pluginContext.irBuiltIns.stringType,
                        JsonObject(
                            "injectionFunctions" to JsonArray(
                                injectablesAndTheirInjectionFunctions
                                    .map { it.injectionFunction }
                                    .filter { it.visibility.isPublicAPI }
                                    .map { it.signatureAsInjectionFunction().parseToJsonObject() }
                                    .toMutableList()
                            ),
                            "services" to JsonArray()
                        ).toString(),
                    )
                }
                function.body = pluginContext.irFactory.createBlockBody(
                    startOffset = SYNTHETIC_OFFSET,
                    endOffset = SYNTHETIC_OFFSET,
                )
            })
        })
    }
}


private fun packageFragmentWithOnlyASingleFunction(
    moduleDescriptor: ModuleDescriptor,
    callableId: CallableId,
): PackageFragmentDescriptorImpl = object : PackageFragmentDescriptorImpl(moduleDescriptor, callableId.packageName) {
    override fun getMemberScope(): MemberScope = object : MemberScope by MemberScope.Empty {
        override fun getFunctionNames(): Set<Name> = setOf(callableId.callableName)
    }
}

private fun MessageCollector.reportErrorsForCycle(cycle: List<IrFunction>) {
    cycle.singleOrNull()?.let { selfDependingInjectable ->
        report(
            CompilerMessageSeverity.ERROR,
            "This dependency depends on itself", // TODO: Add better description in case it's subtype.
            selfDependingInjectable.getCompilerMessageLocation(selfDependingInjectable.file),
        )
        return
    }

    cycle
        .filter { it.isInCurrentModule() }
        .forEach { cycleElement ->
            val shiftedCycle = cycle.shiftedSoThatItStartsWith(cycleElement)
            report(
                CompilerMessageSeverity.ERROR,
                "Dependency cycle found: ${
                    shiftedCycle.joinToString(" -> ") { it.returnType.render() }
                }. Remove this argument ${
                    if (shiftedCycle.last().isInCurrentModule()) ""
                    else ", or make sure ${shiftedCycle.last().returnType.render()} does not depend on this"
                }${
                    if (shiftedCycle.size < 3) ""
                    else ", or break the cycle elsewhere"
                }",
                cycleElement.getCompilerMessageLocation(cycleElement.file),
            )
        }
}

// TODO: Introduce error for ambiguous type dependency
// TODO: IDE idea: If IDE sees someone reference ambiguous type. But it can be disambiguated with narrower type, propose the fix.

private fun MessageCollector.reportErrorsForDuplicates(duplicates: List<IrFunction>) {
    // TODO: Handle duplicates exclusively defined in external modules
    duplicates
        .filter { it.isInCurrentModule() }
        .forEach { duplication ->
            report(
                CompilerMessageSeverity.ERROR,
                "Multiple injectables found with the same return type: ${
                    duplicates
                        .map { it.callableId.callableName }
                        .takeIf { it.size == it.toSet().size }
                        .let { it ?: duplicates.map { it.callableId.asFqNameForDebugInfo() } }
                        .joinToString(", ")
                }. Only one is allowed for a single type.", // TODO: Message that they might be able to remove declaration, or otherwise module?
                duplication.getCompilerMessageLocation(duplication.file),
            )
        }
}

fun <T> List<T>.shiftedSoThatItStartsWith(startElement: T): List<T> =
    listOf(startElement) +
        dropWhile { it != startElement } +
        takeWhile { it != startElement }

private fun IrPluginContext.referenceAllInjectablesDeclaredInDependencyModulesOf(
    moduleFragment: IrModuleFragment,
): List<IrSimpleFunction> = moduleFragment
    .descriptor
    .let { it as FirModuleDescriptor }
    .moduleData
    .dependencies
    .mapNotNull { module -> referenceMetadataFromModuleOrNullIfItsNotSinkModule(module.stableOrRegularName()) }
    .flatMap { metadata ->
        metadata.exposedInjectables.map { injectable ->
            referenceFunctions(CallableId(
                packageName = injectable.parent(),
                className = null,
                callableName = injectable.shortName(),
            ))
                .single() // TODO: Handle multiple (overload declared by user)
                .owner
        }
    }

internal typealias GraphMap = Map<IrFunction, List<ResolvedDependency>>

internal fun GraphMap.mapDependenciesRecursivelyMemoized(
    mapper: (
        function: IrFunction,
        oldDependencies: List<ResolvedDependency>,
        recurse: (IrFunction) -> List<ResolvedDependency>,
    ) -> List<ResolvedDependency>,
): GraphMap {
    val newMap = HashMap<IrFunction, List<ResolvedDependency>>(this.size)
    val visited = HashSet<IrFunction>()
    fun recurse(irFunction: IrFunction): List<ResolvedDependency> {
        return newMap.computeIfAbsent(irFunction) {
            val dependencies = this[irFunction]!!
            if (!visited.add(irFunction)) dependencies // Uh-oh! We encountered a cycle. In theory that might be introduced this mapping operation. We will just return the old values
            else mapper(irFunction, dependencies, ::recurse)
        }
    }

    return mapValues { (it, _) -> recurse(it) }
}

internal sealed class GraphBuildResult {
    data class NoIssues(val graph: GraphMap): GraphBuildResult()
    data class CyclesFound(val cycles: List<List<IrFunction>>): GraphBuildResult()
    data class DuplicatesFound(val duplicates: List<List<IrFunction>>): GraphBuildResult()
}

internal fun GraphBuildResult.NoIssues.allMissingDependenciesOf(
    function: IrFunction,
): List<ResolvedDependency.MissingDependency> = graph[function]?.allMissingDependencies() ?: emptyList()

private fun List<ResolvedDependency>.allMissingDependencies(): List<ResolvedDependency.MissingDependency> =
    flatMap { dependency ->
        when (dependency) {
            is ResolvedDependency.MatchFound -> dependency.indirectDependencies.allMissingDependencies()
            is ResolvedDependency.MissingDependency -> listOf(dependency)
        }
    }

internal fun GraphBuildResult.mapSuccess(
    transform: (GraphBuildResult.NoIssues) -> GraphBuildResult
): GraphBuildResult = when (this) {
    is GraphBuildResult.CyclesFound,
    is GraphBuildResult.DuplicatesFound -> this
    is GraphBuildResult.NoIssues -> transform(this)
}

internal sealed class ResolvedDependency {
    data class MatchFound(
        val parameterName: Name,
        val instantiatorFunction: IrFunction,
        val indirectDependencies: List<ResolvedDependency>,
    ): ResolvedDependency()
    data class MissingDependency(val parameterName: Name, val type: IrType): ResolvedDependency()
}

internal val ResolvedDependency.parameterName: Name get() = when (this) {
    is ResolvedDependency.MissingDependency -> parameterName
    is ResolvedDependency.MatchFound -> parameterName
}

internal fun ResolvedDependency.withParameterName(newName: Name): ResolvedDependency = when (this) {
    is ResolvedDependency.MissingDependency -> copy(parameterName = newName)
    is ResolvedDependency.MatchFound -> copy(parameterName = newName)
}

/** @return false if there were no issues and we can continue normally */
private fun List<IrFunction>.buildGraph(
    accessibleInjectables: List<IrFunction>,
    moduleFragment: IrModuleFragment
): GraphBuildResult = GraphBuildResult.NoIssues(buildMap {
    this@buildGraph.forEach { function ->
        this[function] = function.parameters.map { parameter ->
            accessibleInjectables.pickBestCandidateToProvide(parameter.type)
                ?.let { ResolvedDependency.MatchFound(parameter.name, it, indirectDependencies = emptyList()) }
                ?: ResolvedDependency.MissingDependency(parameter.name, parameter.type)
        }
    }
})
    .detectingCycles()
    .withIndirectMissingDependencies(moduleFragment)
    .detectingDuplicates(accessibleInjectables)

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
 *       Baz(foobs)
 *       foobs
 *     )
 *   )
 * )
 */
private fun GraphBuildResult.withIndirectMissingDependencies(
    moduleFragment: IrModuleFragment
): GraphBuildResult = mapSuccess { graphWithoutIssues ->
    graphWithoutIssues.copy(graph = graphWithoutIssues.graph.mapDependenciesRecursivelyMemoized { function, dependencies, recurse ->
        // TODO: If we support more narrow scoped injectables. We should handle it here? Kinda similar to module border crossing
        val names = mutableSetOf<Name>()
        fun List<ResolvedDependency>.resolvingCrossModuleDependencies(
            moduleThatResolvedThisDependency: IrModuleFragment?,
        ): List<ResolvedDependency> = mapNotNull { indirectDependency ->
            when (indirectDependency) {
                is ResolvedDependency.MatchFound -> indirectDependency.copy(
                    indirectDependencies = recurse(indirectDependency.instantiatorFunction)
                        .resolvingCrossModuleDependencies(indirectDependency.instantiatorFunction.moduleFragment),
                )
                is ResolvedDependency.MissingDependency -> {
                    if (moduleThatResolvedThisDependency != moduleFragment) {
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
                                //    - Had a dependency that it was not able to satisfy in
                                //      their own module (AKA, missing dependency)
                                //    - We were able to satisfy this dependency in our own module
                                // The newly added dependency might again have its own missing dependencies.
                                // So we recurse down its missing dependencies as well.
                                ResolvedDependency.MatchFound(
                                    parameterName = indirectDependency.parameterName,
                                    instantiatorFunction = candidateFromThisModule,
                                    indirectDependencies = recurse(candidateFromThisModule)
                                        .resolvingCrossModuleDependencies(candidateFromThisModule.moduleFragment),
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
                .map { Name.identifier(dependency.parameterName.asString() + it) }
                .first { names.add(it) }
                .let { dependency.withParameterName(it) }
        }

        dependencies.resolvingCrossModuleDependencies(function.moduleFragment)
    })
}

private val IrDeclaration.moduleFragment: IrModuleFragment? get() = this.fileOrNull?.module
internal fun IrDeclaration.isDeclaredIn(module: IrModuleFragment): Boolean = moduleFragment == module

private fun GraphBuildResult.detectingDuplicates(accessibleInjectables: List<IrFunction>): GraphBuildResult =
    mapSuccess { graphWithoutIssues ->
        accessibleInjectables
            .map { it.returnType }
            .filterNot(mutableListOf<IrType>()::add)
            .intersect(graphWithoutIssues.graph.keys)
            .toList()
            .ifEmpty { null }
            ?.let { duplicateTypes ->
                GraphBuildResult.DuplicatesFound(duplicateTypes.map {
                        type -> graphWithoutIssues.graph.keys.filter { it.returnType == type }
                })
            }
            ?: this
    }

private fun GraphBuildResult.detectingCycles(): GraphBuildResult = mapSuccess { graphWithoutIssues ->
    graphWithoutIssues.graph.findCycles().ifEmpty { null }?.let(GraphBuildResult::CyclesFound) ?: this
}

private fun Iterable<IrFunction>.pickBestCandidateToProvide(type: IrType): IrFunction? =
    singleOrNull { it.returnType == type }

private fun collectInjectableImplementationsOf(moduleFragment: IrModuleFragment): List<IrFunction> = buildList {
    moduleFragment.acceptChildrenVoid(object : IrVisitorVoid() {
        override fun visitElement(element: IrElement) {
            element.acceptChildrenVoid(this)
        }

        override fun visitFunction(declaration: IrFunction) {
            if (!declaration.declaresInjectable()) return
            add(declaration)
        }
    })
}

private fun IrFunction.declaresInjectable(): Boolean = hasAnnotation(injectableAnnotationFqn) ||
    (this is IrConstructor && this.parentAsClass.hasAnnotation(injectableAnnotationFqn))

private data class SinkModuleMetadata(
    val exposedInjectables: List<FqName>,
)

/**
 * TODO
 */
private fun IrPluginContext.referenceMetadataFromModuleOrNullIfItsNotSinkModule(moduleName: Name): SinkModuleMetadata? {
    val metadataFunction = referenceFunctions(moduleName.getModuleMetadataFunctionId()).singleOrNull() ?: run {
        // TODO: Log?
        return null
    }

    metadataFunction
        .owner
        .annotations
        .firstOrNull()
        .let { it ?: error("Sink metadata from module $moduleName, has incompatible metadata format") }
        .also { metadataAnnotation ->
            val annotationName = metadataAnnotation.symbol.owner.parentAsClass.name.identifier
            assert(annotationName == "_SinkMetadata") {
                "Somehow metadata annotation from $moduleName is not a _SinkMetadata class, but instead $annotationName"
            }
        }
        .arguments
        .firstOrNull()
        .let { it ?: error("Sink metadata from module $moduleName, has it's property and annotation, but no value") }
        .let { it as IrConstantPrimitive }

    TODO()
}

private fun ModuleDescriptor.stableOrRegularName(): Name = stableName ?: name
private fun FirModuleData.stableOrRegularName(): Name = stableModuleName?.let(Name::identifier)
    ?: name.asString().removeSurrounding("<regular dependencies of <", ">>").let(Name::identifier)

private fun Name.getModuleMetadataFunctionId(): CallableId = CallableId(
    packageName = FqName("${asString().asValidJavaIdentifier()}.__sink_metadata__"),
    className = null,
    callableName = Name.identifier("sinkMetadata"),
)

private val injectableAnnotationFqn = FqName("org.jetbrains.kotlin.compiler.plugin.template.Injectable")
private val metadataAnnotationFqn = FqName("org.jetbrains.kotlin.compiler.plugin.template._SinkMetadata")
private val injectionCacheFqn = FqName("org.jetbrains.kotlin.compiler.plugin.template.InjectionCache")

private fun String.asValidJavaIdentifier(): String {
    val first = if (this.first().isJavaIdentifierStart()) this.first() else '_'
    return first + this.drop(1).map { if (it.isJavaIdentifierPart()) it else '_' }.joinToString("")
}

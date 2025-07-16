package org.jetbrains.kotlin.compiler.plugin.template.ir

// TODO: Ensure Nullability handled
// TODO: Add support for kx Serializer injection? Might be cool icw generic support

import org.jetbrains.kotlin.backend.common.extensions.IrGenerationExtension
import org.jetbrains.kotlin.backend.common.extensions.IrPluginContext
import org.jetbrains.kotlin.backend.common.getCompilerMessageLocation
import org.jetbrains.kotlin.backend.jvm.ir.isInCurrentModule
import org.jetbrains.kotlin.cli.common.messages.CompilerMessageSeverity
import org.jetbrains.kotlin.cli.common.messages.MessageCollector
import org.jetbrains.kotlin.descriptors.ModuleDescriptor
import org.jetbrains.kotlin.ir.IrElement
import org.jetbrains.kotlin.ir.declarations.IrConstructor
import org.jetbrains.kotlin.ir.declarations.IrDeclaration
import org.jetbrains.kotlin.ir.declarations.IrFunction
import org.jetbrains.kotlin.ir.declarations.IrModuleFragment
import org.jetbrains.kotlin.ir.declarations.IrSimpleFunction
import org.jetbrains.kotlin.ir.expressions.IrConstantPrimitive
import org.jetbrains.kotlin.ir.types.IrType
import org.jetbrains.kotlin.ir.types.typeWith
import org.jetbrains.kotlin.ir.util.addChild
import org.jetbrains.kotlin.ir.util.file
import org.jetbrains.kotlin.ir.util.fileOrNull
import org.jetbrains.kotlin.ir.util.functions
import org.jetbrains.kotlin.ir.util.hasAnnotation
import org.jetbrains.kotlin.ir.util.parentAsClass
import org.jetbrains.kotlin.ir.util.render
import org.jetbrains.kotlin.ir.visitors.IrVisitorVoid
import org.jetbrains.kotlin.ir.visitors.acceptChildrenVoid
import org.jetbrains.kotlin.name.CallableId
import org.jetbrains.kotlin.name.ClassId
import org.jetbrains.kotlin.name.FqName
import org.jetbrains.kotlin.name.Name

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

        graphWithoutIssues.graph.keys.forEach { injectable ->
            injectable.file.addChild(
                creationSession.generateInjectionFunction(injectable, graphWithoutIssues)
            )
        }
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
            val shiftedCycle = cycle.startingWith(cycleElement)
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

private fun MessageCollector.reportErrorsForDuplicates(duplicates: List<IrFunction>) {
    duplicates
        .filter { it.isInCurrentModule() }
        .forEach { duplication ->
            val shiftedCycle = duplicates.startingWith(duplication)
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
                duplication.getCompilerMessageLocation(duplication.file),
            )
        }
}

fun <T> List<T>.startingWith(startElement: T): List<T> =
    listOf(startElement) +
        dropWhile { it != startElement } +
        takeWhile { it != startElement }

private fun IrPluginContext.referenceAllInjectablesDeclaredInDependencyModulesOf(
    moduleFragment: IrModuleFragment,
): List<IrSimpleFunction> = moduleFragment
    .descriptor
    .allDependencyModules
    .mapNotNull { module -> referenceMetadataFromModuleOrNullIfItsNotSinkModule(module) }
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

internal sealed class GraphBuildResult {
    class NoIssues(val graph: Map<IrFunction, List<ResolvedDependency>>): GraphBuildResult()
    class CyclesFound(val cycles: List<List<IrFunction>>): GraphBuildResult()
    class DuplicatesFound(val duplicates: List<List<IrFunction>>): GraphBuildResult()
}

internal fun GraphBuildResult.mapSuccess(
    transform: (GraphBuildResult.NoIssues) -> GraphBuildResult
): GraphBuildResult = when (this) {
    is GraphBuildResult.CyclesFound,
    is GraphBuildResult.DuplicatesFound -> this
    is GraphBuildResult.NoIssues -> transform(this)
}

internal sealed class ResolvedDependency {
    data class MatchFound(val parameterName: Name, val instantiatorFunction: IrFunction): ResolvedDependency()
    data class MissingDependency(val parameterName: Name, val type: IrType): ResolvedDependency()
}

internal fun ResolvedDependency.typeOrNull(): IrType? = when (this) {
    is ResolvedDependency.MissingDependency -> type
    is ResolvedDependency.MatchFound -> null
}

/** @return false if there were no issues and we can continue normally */
private fun List<IrFunction>.buildGraph(
    accessibleInjectables: List<IrFunction>,
    moduleFragment: IrModuleFragment
): GraphBuildResult = GraphBuildResult.NoIssues(buildMap {
    this@buildGraph.forEach { function ->
        this[function] = function.parameters.map { parameter ->
            accessibleInjectables.pickBestCandidateToProvide(parameter.type)
                ?.let {ResolvedDependency.MatchFound(parameter.name, it) }
                ?: ResolvedDependency.MissingDependency(parameter.name, parameter.type)
        }
    }
})
    .detectingCycles()
    .withIndirectMissingDependencies(moduleFragment)
    .detectingDuplicates(accessibleInjectables)

private fun GraphBuildResult.withIndirectMissingDependencies(
    moduleFragment: IrModuleFragment
): GraphBuildResult = mapSuccess { graphWithoutIssues ->
    val newGraph = graphWithoutIssues.graph.mapValues { (_, dependencies) -> dependencies.toMutableList() }
    fun GraphBuildResult.NoIssues.addDepthFirstToNewMapAndReturnMissingDependencies(
        function: IrFunction,
    ): List<ResolvedDependency.MissingDependency> =
        graphWithoutIssues.graph[function]?.flatMap { dependency ->
            when (dependency) {
                is ResolvedDependency.MatchFound -> {
                    val isModuleCrossingDependency = !dependency.instantiatorFunction.isDeclaredIn(moduleFragment)
                        && function.isDeclaredIn(moduleFragment)
                    // TODO: If we support more narrow scoped injectables. We should handle it here? Kinda similar to module border crossing
                    addDepthFirstToNewMapAndReturnMissingDependencies(dependency.instantiatorFunction)
                        .mapNotNull { indirectDependency ->
                            if (isModuleCrossingDependency) {
                                // Try to resolve the dependency in our own module!
                                graphWithoutIssues
                                    .graph
                                    .keys
                                    .pickBestCandidateToProvide(indirectDependency.type) // Try to resolve within this module
                                    ?.let { candidateFromThisModule ->
                                        // Yay! We were able to resolve a dependency more locally (in module) than the module that declared it
                                        val currentDeps = newGraph[function]!!
                                        if (currentDeps.none { it.typeOrNull() == indirectDependency.type }) {
                                            // TODO: Handle parameter name duplication
                                            // After making sure that we're not already in the dependencies, we can add the new one
                                            currentDeps += ResolvedDependency.MatchFound(
                                                parameterName = indirectDependency.parameterName,
                                                instantiatorFunction = candidateFromThisModule,
                                            )
                                        }
                                        return@mapNotNull null // So we don't need to handle it in our usages anymore.
                                    } ?: indirectDependency
                            } else {
                                indirectDependency
                            }
                        }
                }
                is ResolvedDependency.MissingDependency -> listOf(
                    ResolvedDependency.MissingDependency(dependency.parameterName, dependency.type),
                )
            }
        } ?: emptyList()

    graphWithoutIssues.graph.keys.forEach { graphWithoutIssues.addDepthFirstToNewMapAndReturnMissingDependencies(it) }

    GraphBuildResult.NoIssues(newGraph)
}

internal fun IrDeclaration.isDeclaredIn(module: IrModuleFragment): Boolean = this.fileOrNull?.module == module

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
private fun IrPluginContext.referenceMetadataFromModuleOrNullIfItsNotSinkModule(
    module: ModuleDescriptor,
): SinkModuleMetadata? {
    val moduleName = module.stableName ?: module.name
    val callableId = module.getMetadataPropertyId()
    val metadataProperty = referenceProperties(callableId)
        .singleOrNull() ?: run {
            // TODO: Log?
            return null
        }

    metadataProperty
        .owner
        .annotations
        .firstOrNull()
        .let { it ?: error("Sink metadata from module $moduleName, has incompatible metadata format") }
        .also { metadataAnnotation ->
            val annotationName = metadataAnnotation.symbol.owner.parentAsClass.name.identifier
            assert(annotationName == "_SinkMetadata") {
                "Somehow metadata annotation from $module is not a _SinkMetadata class, but instead $annotationName"
            }
        }
        .arguments
        .firstOrNull()
        .let { it ?: error("Sink metadata from module $moduleName, has it's property and annotation, but no value") }
        .let { it as IrConstantPrimitive }

    TODO()
}

private fun ModuleDescriptor.getMetadataPropertyId(): CallableId {
    val moduleNameAsPackageName = (stableName ?: this.name).identifier.asValidJavaIdentifier()
    return CallableId(
        packageName = FqName("$moduleNameAsPackageName.__sink_metadata__"),
        className = null,
        callableName = Name.identifier("sinkMetadata"),
    )
}

private val injectableAnnotationFqn = FqName("org.jetbrains.kotlin.compiler.plugin.template.Injectable")
private val injectionCacheFqn = FqName("org.jetbrains.kotlin.compiler.plugin.template.InjectionCache")

private fun String.asValidJavaIdentifier(): String {
    val first = if (this.first().isJavaIdentifierStart()) this.first() else '_'
    return first + this.drop(1).map { if (it.isJavaIdentifierPart()) it else '_' }.joinToString("")
}

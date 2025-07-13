package org.jetbrains.kotlin.compiler.plugin.template.ir

// TODO: Ensure Nullability handled

import org.jetbrains.kotlin.backend.common.extensions.IrGenerationExtension
import org.jetbrains.kotlin.backend.common.extensions.IrPluginContext
import org.jetbrains.kotlin.backend.common.getCompilerMessageLocation
import org.jetbrains.kotlin.backend.jvm.ir.isInCurrentModule
import org.jetbrains.kotlin.cli.common.messages.CompilerMessageSeverity
import org.jetbrains.kotlin.cli.common.messages.MessageCollector
import org.jetbrains.kotlin.compiler.plugin.template.ir.GraphBuildResult.CyclesFound
import org.jetbrains.kotlin.descriptors.ModuleDescriptor
import org.jetbrains.kotlin.ir.IrElement
import org.jetbrains.kotlin.ir.declarations.IrConstructor
import org.jetbrains.kotlin.ir.declarations.IrDeclarationWithName
import org.jetbrains.kotlin.ir.declarations.IrFunction
import org.jetbrains.kotlin.ir.declarations.IrModuleFragment
import org.jetbrains.kotlin.ir.declarations.IrSimpleFunction
import org.jetbrains.kotlin.ir.expressions.IrConstantPrimitive
import org.jetbrains.kotlin.ir.types.IrDynamicType
import org.jetbrains.kotlin.ir.types.IrErrorType
import org.jetbrains.kotlin.ir.types.IrSimpleType
import org.jetbrains.kotlin.ir.types.IrType
import org.jetbrains.kotlin.ir.types.SimpleTypeNullability.MARKED_NULLABLE
import org.jetbrains.kotlin.ir.types.typeOrNull
import org.jetbrains.kotlin.ir.util.file
import org.jetbrains.kotlin.ir.util.fqNameWhenAvailable
import org.jetbrains.kotlin.ir.util.getPackageFragment
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
        val injectablesDeclaredInThisModule = collectInjectableImplementationsOf(moduleFragment)
        if (injectablesDeclaredInThisModule.isEmpty()) return

        val injectablesDeclaredInDependingModules = pluginContext
            .referenceAllInjectablesDeclaredInDependencyModulesOf(moduleFragment)

        val accessibleInjectables = injectablesDeclaredInDependingModules + injectablesDeclaredInThisModule
        val graph = when (val result = injectablesDeclaredInThisModule.buildGraph(accessibleInjectables)) {
            is CyclesFound -> {
                result.cycles.forEach { cycle ->
                    pluginContext.messageCollector.reportErrorsForCycle(cycle)
                }
                return
            }
            is GraphBuildResult.NoCycles -> result.graph
        }

        graph.forEach { (injectable, dependencies) ->
            val visibility = injectable
                .returnType
                .getClassIds()
                .map { pluginContext.referenceClass(it)!!.owner.visibility }
                .sortedWith { a, b -> a.compareTo(b) ?: Int.MAX_VALUE }
                .last()


        }
    }
}

private fun IrType.getClassIds(): List<ClassId> = buildList {
    fun IrType.visit() {
        when (this) {
            is IrDynamicType -> TODO()
            is IrErrorType -> TODO()
            is IrSimpleType -> {
                add(ClassId(
                    packageFqName = classifier.owner.getPackageFragment()?.packageFqName ?: return,
                    relativeClassName = (classifier.owner as? IrDeclarationWithName)?.fqNameWhenAvailable ?: return,
                    isLocal = false,
                ))
                arguments.forEach { it.typeOrNull?.visit() }
            }
        }
    }
    this@getClassIds.visit()
}

private fun IrType.asFunctionName(): String = when (this) {
    is IrDynamicType -> TODO()
    is IrErrorType -> TODO()
    is IrSimpleType -> (this.classifier.owner as IrDeclarationWithName).name.asStringStripSpecialMarkers() +
        arguments.joinToString(separator = "And", prefix = "Of") { it.typeOrNull?.asFunctionName() ?: "Unknown" } +
        if (nullability == MARKED_NULLABLE) "OrNull" else ""
}

private fun MessageCollector.reportErrorsForCycle(cycle: List<IrFunction>) {
    fun cycleStartingWith(startElement: IrFunction): List<IrFunction> =
        listOf(startElement) +
            cycle.dropWhile { it != startElement } +
            cycle.takeWhile { it != startElement }

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
            val shiftedCycle = cycleStartingWith(cycleElement)
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

private sealed class GraphBuildResult {
    class NoCycles(val graph: Map<IrFunction, List<IrFunction?>>): GraphBuildResult()
    class CyclesFound(val cycles: List<List<IrFunction>>): GraphBuildResult()
}

/** @return false if there were no issues and we can continue normally */
private fun List<IrFunction>.buildGraph(
    accessibleInjectables: List<IrFunction>
): GraphBuildResult = GraphBuildResult.NoCycles(buildMap {
    this@buildGraph.forEach { function ->
        this[function] = function.parameters.map { parameter ->
            accessibleInjectables.pickBestCandidateToProvide(parameter.type)
        }
    }
}).detectingCycles()

private fun GraphBuildResult.NoCycles.detectingCycles(): GraphBuildResult =
    graph.findCycles().ifEmpty { null }?.let(::CyclesFound) ?: this

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

private fun String.asValidJavaIdentifier(): String {
    val first = if (this.first().isJavaIdentifierStart()) this.first() else '_'
    return first + this.drop(1).map { if (it.isJavaIdentifierPart()) it else '_' }.joinToString("")
}

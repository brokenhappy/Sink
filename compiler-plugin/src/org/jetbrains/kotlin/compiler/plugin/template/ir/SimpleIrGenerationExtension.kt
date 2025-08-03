package org.jetbrains.kotlin.compiler.plugin.template.ir

// TODO: Ensure Nullability handled
// TODO: Add support for kx Serializer injection? Might be cool icw generic support

import com.woutwerkman.sink.ide.plugin.common.DependencyGraphBuilder
import com.woutwerkman.sink.ide.plugin.common.moduleDependencyGraphFromBytes
import org.jetbrains.kotlin.backend.common.extensions.IrGeneratedDeclarationsRegistrar
import org.jetbrains.kotlin.backend.common.extensions.IrGenerationExtension
import org.jetbrains.kotlin.backend.common.extensions.IrPluginContext
import org.jetbrains.kotlin.backend.common.getCompilerMessageLocation
import org.jetbrains.kotlin.backend.jvm.ir.isInCurrentModule
import org.jetbrains.kotlin.cli.common.messages.CompilerMessageSeverity
import org.jetbrains.kotlin.cli.common.messages.MessageCollector
import org.jetbrains.kotlin.compiler.plugin.template.metadataFunctionCallableId
import org.jetbrains.kotlin.compiler.plugin.template.somewhatIdentifyingName
import org.jetbrains.kotlin.descriptors.ClassKind
import org.jetbrains.kotlin.descriptors.DescriptorVisibilities
import org.jetbrains.kotlin.descriptors.Modality
import org.jetbrains.kotlin.descriptors.ModuleDescriptor
import org.jetbrains.kotlin.descriptors.impl.PackageFragmentDescriptorImpl
import org.jetbrains.kotlin.fir.FirModuleData
import org.jetbrains.kotlin.fir.descriptors.FirModuleDescriptor
import org.jetbrains.kotlin.ir.IrElement
import org.jetbrains.kotlin.ir.declarations.IrConstructor
import org.jetbrains.kotlin.ir.declarations.IrDeclarationOrigin
import org.jetbrains.kotlin.ir.declarations.IrFunction
import org.jetbrains.kotlin.ir.declarations.IrModuleFragment
import org.jetbrains.kotlin.ir.declarations.IrParameterKind
import org.jetbrains.kotlin.ir.declarations.IrSimpleFunction
import org.jetbrains.kotlin.ir.declarations.IrValueParameter
import org.jetbrains.kotlin.ir.declarations.impl.IrFileImpl
import org.jetbrains.kotlin.ir.expressions.IrConstantPrimitive
import org.jetbrains.kotlin.ir.expressions.IrStatementOrigin
import org.jetbrains.kotlin.ir.expressions.impl.IrConstructorCallImpl
import org.jetbrains.kotlin.ir.expressions.impl.IrThrowImpl
import org.jetbrains.kotlin.ir.symbols.IrFunctionSymbol
import org.jetbrains.kotlin.ir.symbols.impl.IrClassSymbolImpl
import org.jetbrains.kotlin.ir.symbols.impl.IrFileSymbolImpl
import org.jetbrains.kotlin.ir.symbols.impl.IrSimpleFunctionSymbolImpl
import org.jetbrains.kotlin.ir.symbols.impl.IrValueParameterSymbolImpl
import org.jetbrains.kotlin.ir.types.IrTypeSystemContextImpl
import org.jetbrains.kotlin.ir.types.typeWith
import org.jetbrains.kotlin.ir.util.NaiveSourceBasedFileEntryImpl
import org.jetbrains.kotlin.ir.util.SYNTHETIC_OFFSET
import org.jetbrains.kotlin.ir.util.addChild
import org.jetbrains.kotlin.ir.util.addFile
import org.jetbrains.kotlin.ir.util.callableId
import org.jetbrains.kotlin.ir.util.constructors
import org.jetbrains.kotlin.ir.util.file
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
import org.jetbrains.kotlin.resolve.scopes.MemberScope

class SimpleIrGenerationExtension: IrGenerationExtension {
    override fun generate(moduleFragment: IrModuleFragment, pluginContext: IrPluginContext) {
        context(
            IrTypeBehavior(IrTypeSystemContextImpl(pluginContext.irBuiltIns)),
            functionBehavior,
            pluginContext.metadataDeclarationRegistrar,
        ) {
            generateInternal(moduleFragment, pluginContext)
        }
    }

    context(_: TypeBehavior, _: FunctionBehavior, metadataDeclarationRegistrar: IrGeneratedDeclarationsRegistrar)
    private fun generateInternal(moduleFragment: IrModuleFragment, pluginContext: IrPluginContext) {
        // TODO: Use explicit-API compiler flag for some things?
        val injectableCacheInterface = pluginContext.referenceClass(ClassId.topLevel(injectionCacheFqn))
            ?: return // Early exit, this module does not include Sink compile time dependency
        val injectablesDeclaredInThisModule = collectInjectableImplementationsOf(moduleFragment)
        if (injectablesDeclaredInThisModule.isEmpty()) return
        val injectableCacheType by lazy { injectableCacheInterface.typeWith() }

        val moduleDependencyGraphs = pluginContext.referenceAllModuleDependencyGraphs()

        val graph = DependencyGraphBuilder().buildGraph(injectablesDeclaredInThisModule, moduleDependencyGraphs)

        graph.cycles.forEach { cycle ->
            pluginContext.messageCollector.reportErrorsForCycle(cycle)
        }
        graph.duplicates.forEach { duplicates ->
            pluginContext.messageCollector.reportErrorsForDuplicates(duplicates)
        }

        val creationSession = InjectionFunctionCreationSession(
            pluginContext,
            moduleFragment,
            injectableCacheType,
            injectionCacheComputeIfAbsentMethodSymbol = injectableCacheInterface.functions.first().owner.symbol,
        )

        data class InjectableAndInjectionFunction(val injectable: IrFunction, val injectionFunction: IrSimpleFunction)

        val injectablesAndTheirInjectionFunctions = graph
            .instantiatorFunctionsToDependencies
            .keys
            .map { injectable ->
                InjectableAndInjectionFunction(
                    injectable = injectable.owner,
                    injectionFunction = creationSession.generateInjectionFunction(injectable.owner, graph),
                )
            }

        // Now persist the generated declarations
        injectablesAndTheirInjectionFunctions.forEach { (injectable, injectionFunction) ->
            injectable.file.addChild(injectionFunction)
        }

        val callableId = moduleFragment.descriptor.stableOrRegularName().getModuleMetadataFunctionId()
        moduleFragment.addFile(IrFileImpl(
            fileEntry = NaiveSourceBasedFileEntryImpl(
                name = "__sink_metadata__",
            ),
            symbol = IrFileSymbolImpl(
                packageFragmentWithOnlyASingleFunction(moduleFragment.descriptor, callableId)
            ),
            fqName = callableId.packageName,
            module = moduleFragment,
        ).also { file ->
            val uniqueInterfaceSymbolName = IrClassSymbolImpl()
            pluginContext.irFactory.createClass(
                startOffset = SYNTHETIC_OFFSET,
                endOffset = SYNTHETIC_OFFSET,
                origin = IrDeclarationOrigin.GeneratedByPlugin(SinkPluginKey),
                name = Name.identifier(
                    (moduleFragment.descriptor as FirModuleDescriptor).moduleData.somewhatIdentifyingName()
                ),
                visibility = DescriptorVisibilities.PUBLIC,
                uniqueInterfaceSymbolName,
                kind = ClassKind.INTERFACE,
                modality = Modality.OPEN,
            ).also { interfaceClass ->
                interfaceClass.parent = file
                file.addChild(interfaceClass)
            }
            pluginContext.irFactory.createSimpleFunction(
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
                file.addChild(function)
                function.parent = file
                function.parameters += pluginContext.irFactory.createValueParameter(
                    startOffset = SYNTHETIC_OFFSET,
                    endOffset = SYNTHETIC_OFFSET,
                    origin = IrDeclarationOrigin.GeneratedByPlugin(SinkPluginKey),
                    name = Name.identifier("a"),
                    type = uniqueInterfaceSymbolName.typeWith(),
                    kind = IrParameterKind.Regular,
                    isAssignable = false,
                    symbol = IrValueParameterSymbolImpl(),
                    varargElementType = null,
                    isCrossinline = false,
                    isNoinline = false,
                    isHidden = false,
                ).also { it.parent = function }
                metadataDeclarationRegistrar
                    .registerFunctionAsMetadataVisible(function)
                metadataDeclarationRegistrar
                    .addCustomMetadataExtension(function, compilerPluginId, graph.serializeAsModuleDependencyGraph())
                function.body = pluginContext.irFactory.createExpressionBody(
                    startOffset = SYNTHETIC_OFFSET,
                    endOffset = SYNTHETIC_OFFSET,
                    expression = IrThrowImpl(
                        startOffset = SYNTHETIC_OFFSET,
                        endOffset = SYNTHETIC_OFFSET,
                        type = pluginContext.irBuiltIns.throwableType,
                        value = IrConstructorCallImpl(
                            startOffset = SYNTHETIC_OFFSET,
                            endOffset = SYNTHETIC_OFFSET,
                            pluginContext.irBuiltIns.throwableType,
                            pluginContext.irBuiltIns.throwableClass.constructors.first { it.owner.parameters.isEmpty() },
                            typeArgumentsCount = 0,
                            constructorTypeArgumentsCount = 0,
                        )
                    ),
                )
            }
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

private fun MessageCollector.reportErrorsForCycle(cycle: List<IrFunctionSymbol>) {
    cycle.singleOrNull()?.let { selfDependingInjectable ->
        report(
            CompilerMessageSeverity.ERROR,
            "This dependency depends on itself", // TODO: Add better description in case it's subtype.
            selfDependingInjectable.owner.getCompilerMessageLocation(selfDependingInjectable.owner.file),
        )
        return
    }

    cycle
        .filter { it.owner.isInCurrentModule() }
        .forEach { cycleElement ->
            val shiftedCycle = cycle.shiftedSoThatItStartsWith(cycleElement)
            report(
                CompilerMessageSeverity.ERROR,
                "Dependency cycle found: ${
                    shiftedCycle.joinToString(" -> ") { it.owner.returnType.render() }
                }. Remove this argument ${
                    if (shiftedCycle.last().owner.isInCurrentModule()) ""
                    else ", or make sure ${shiftedCycle.last().owner.returnType.render()} does not depend on this"
                }${
                    if (shiftedCycle.size < 3) ""
                    else ", or break the cycle elsewhere"
                }",
                cycleElement.owner.getCompilerMessageLocation(cycleElement.owner.file),
            )
        }
}

// TODO: Introduce error for ambiguous type dependency
// TODO: IDE idea: If IDE sees someone reference ambiguous type. But it can be disambiguated with narrower type, propose the fix.

private fun MessageCollector.reportErrorsForDuplicates(duplicates: List<IrFunctionSymbol>) {
    // TODO: Handle duplicates exclusively defined in external modules
    duplicates
        .filter { it.owner.isInCurrentModule() }
        .forEach { duplication ->
            report(
                CompilerMessageSeverity.ERROR,
                "Multiple injectables found with the same return type: ${
                    duplicates
                        .map { it.owner.callableId.callableName }
                        .takeIf { it.size == it.toSet().size }
                        .let { it ?: duplicates.map { it.owner.callableId.asFqNameForDebugInfo() } }
                        .joinToString(", ")
                }. Only one is allowed for a single type.", // TODO: Message that they might be able to remove declaration, or otherwise module?
                duplication.owner.getCompilerMessageLocation(duplication.owner.file),
            )
        }
}

fun <T> List<T>.shiftedSoThatItStartsWith(startElement: T): List<T> =
    listOf(startElement) +
        dropWhile { it != startElement } +
        takeWhile { it != startElement }

private const val compilerPluginId = "com.woutwerkman.Sink"

context(metadataDeclarationRegistrar: IrGeneratedDeclarationsRegistrar, _: TypeBehavior, _: FunctionBehavior)
private fun IrPluginContext.referenceAllModuleDependencyGraphs(): List<ModuleDependencyGraph> =
    referenceFunctions(metadataFunctionCallableId).mapNotNull { metadataFunction ->
        metadataDeclarationRegistrar
            .getCustomMetadataExtension(metadataFunction.owner, compilerPluginId)
            ?.let { metadata ->
                moduleDependencyGraphFromBytes(
                    functionResolver = { referenceFunctions(FqName(it).asCallableId()).single() },
                    metadata,
                )
            }
    }

private fun collectInjectableImplementationsOf(moduleFragment: IrModuleFragment): List<IrFunctionSymbol> = buildList {
    moduleFragment.acceptChildrenVoid(object : IrVisitorVoid() {
        override fun visitElement(element: IrElement) {
            element.acceptChildrenVoid(this)
        }

        override fun visitFunction(declaration: IrFunction) {
            if (!declaration.declaresInjectable()) return
            add(declaration.symbol)
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
private fun IrPluginContext.referenceMetadataFromModuleNameOrNullIfItsNotSinkModule(moduleName: Name): SinkModuleMetadata? {
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

internal fun String.asValidJavaIdentifier(): String {
    val first = if (this.first().isJavaIdentifierStart()) this.first() else '_'
    return first + this.drop(1).map { if (it.isJavaIdentifierPart()) it else '_' }.joinToString("")
}

private fun FqName.asCallableId(): CallableId = CallableId(
    packageName = parent(),
    className = null,
    callableName = shortName(),
)

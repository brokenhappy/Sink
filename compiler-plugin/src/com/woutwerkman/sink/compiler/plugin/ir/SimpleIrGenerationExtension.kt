package com.woutwerkman.sink.compiler.plugin.ir

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
import com.woutwerkman.sink.compiler.plugin.metadataFunctionCallableId
import com.woutwerkman.sink.compiler.plugin.somewhatIdentifyingName
import org.jetbrains.kotlin.descriptors.ClassKind
import org.jetbrains.kotlin.descriptors.DescriptorVisibilities
import org.jetbrains.kotlin.descriptors.Modality
import org.jetbrains.kotlin.descriptors.ModuleDescriptor
import org.jetbrains.kotlin.descriptors.impl.PackageFragmentDescriptorImpl
import org.jetbrains.kotlin.fir.descriptors.FirModuleDescriptor
import org.jetbrains.kotlin.ir.IrElement
import org.jetbrains.kotlin.ir.declarations.*
import org.jetbrains.kotlin.ir.declarations.impl.IrFileImpl
import org.jetbrains.kotlin.ir.expressions.IrConst
import org.jetbrains.kotlin.ir.expressions.impl.IrConstImpl
import org.jetbrains.kotlin.ir.expressions.impl.IrConstructorCallImpl
import org.jetbrains.kotlin.ir.symbols.IrFunctionSymbol
import org.jetbrains.kotlin.ir.symbols.impl.IrClassSymbolImpl
import org.jetbrains.kotlin.ir.symbols.impl.IrFileSymbolImpl
import org.jetbrains.kotlin.ir.symbols.impl.IrSimpleFunctionSymbolImpl
import org.jetbrains.kotlin.ir.symbols.impl.IrValueParameterSymbolImpl
import org.jetbrains.kotlin.ir.types.IrTypeSystemContextImpl
import org.jetbrains.kotlin.ir.types.classifierOrNull
import org.jetbrains.kotlin.ir.types.typeWith
import org.jetbrains.kotlin.ir.util.*
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
            pluginContext,
        ) {
            generateInternal(moduleFragment)
        }
    }

    context(
        typeBehavior: TypeBehavior,
        _: FunctionBehavior,
        metadataDeclarationRegistrar: IrGeneratedDeclarationsRegistrar,
        pluginContext: IrPluginContext,
    )
    private fun generateInternal(moduleFragment: IrModuleFragment) {
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
            typeBehavior,
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
            metadataDeclarationRegistrar.registerFunctionAsMetadataVisible(injectionFunction)
        }

        val callableId = moduleFragment.descriptor.stableOrRegularName().getModuleMetadataFunctionId()
        val uniqueTypeNameToDisambiguateMetadataFunctionOverloads = Name.identifier(
            (moduleFragment.descriptor as FirModuleDescriptor).moduleData.somewhatIdentifyingName()
        )
        moduleFragment.addFile(IrFileImpl(
            fileEntry = NaiveSourceBasedFileEntryImpl(
                name = "__sink_metadata__",
            ),
            symbol = IrFileSymbolImpl(
                packageFragmentWithOnlyASingleFunctionAndType(
                    moduleDescriptor = moduleFragment.descriptor,
                    callableId = callableId,
                    typeName = uniqueTypeNameToDisambiguateMetadataFunctionOverloads,
                )
            ),
            fqName = callableId.packageName,
            module = moduleFragment,
        ).also { file ->
            val uniqueInterfaceSymbolName = IrClassSymbolImpl()
            pluginContext.irFactory.createClass(
                startOffset = SYNTHETIC_OFFSET,
                endOffset = SYNTHETIC_OFFSET,
                origin = IrDeclarationOrigin.GeneratedByPlugin(SinkPluginKey),
                name = uniqueTypeNameToDisambiguateMetadataFunctionOverloads,
                visibility = DescriptorVisibilities.PUBLIC,
                uniqueInterfaceSymbolName,
                kind = ClassKind.INTERFACE,
                modality = Modality.SEALED,
            ).also { interfaceClass ->
                interfaceClass.parent = file
                interfaceClass.thisReceiver = pluginContext.irFactory.createValueParameter(
                    startOffset = SYNTHETIC_OFFSET,
                    endOffset = SYNTHETIC_OFFSET,
                    origin = IrDeclarationOrigin.GeneratedByPlugin(SinkPluginKey),
                    name = Name.special("<this>"),
                    type = uniqueInterfaceSymbolName.typeWith(),
                    kind = IrParameterKind.Regular,
                    isAssignable = false,
                    symbol = IrValueParameterSymbolImpl(),
                    varargElementType = null,
                    isCrossinline = false,
                    isNoinline = false,
                    isHidden = false,
                ).also { it.parent = interfaceClass }
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
                val metadataAnnotationConstructor = pluginContext.referenceConstructors(
                    ClassId(metadataAnnotationFqn.parent(), metadataAnnotationFqn.shortName())
                ).single()
                metadataDeclarationRegistrar
                    .addMetadataVisibleAnnotationsToElement(
                        pluginContext
                            .referenceFunctions(metadataFunctionCallableId)
                            .first {
                                it
                                    .owner
                                    .parameters
                                    .single()
                                    .type
                                    .classifierOrNull
                                    ?.owner
                                    ?.let { it as? IrClass }
                                    ?.name == uniqueTypeNameToDisambiguateMetadataFunctionOverloads
                            }
                            .owner,
                        listOf(
                            IrConstructorCallImpl(
                                startOffset = SYNTHETIC_OFFSET,
                                endOffset = SYNTHETIC_OFFSET,
                                type = metadataAnnotationConstructor.owner.returnType,
                                symbol = metadataAnnotationConstructor,
                                typeArgumentsCount = 0,
                                constructorTypeArgumentsCount = 0,
                            ).apply {
                                arguments[0] = IrConstImpl.string(
                                    startOffset = SYNTHETIC_OFFSET,
                                    endOffset = SYNTHETIC_OFFSET,
                                    type = pluginContext.irBuiltIns.stringType,
                                    value = graph.serializeAsModuleDependencyGraph().decodeToString(),
                                )
                            },
                        ),
                    )
                function.body = pluginContext.irFactory.createBlockBody(
                    startOffset = SYNTHETIC_OFFSET,
                    endOffset = SYNTHETIC_OFFSET,
                )
            }
        })
    }
}


private fun packageFragmentWithOnlyASingleFunctionAndType(
    moduleDescriptor: ModuleDescriptor,
    callableId: CallableId,
    typeName: Name,
): PackageFragmentDescriptorImpl = object : PackageFragmentDescriptorImpl(moduleDescriptor, callableId.packageName) {
    override fun getMemberScope(): MemberScope = object : MemberScope by MemberScope.Empty {
        override fun getFunctionNames(): Set<Name> = setOf(callableId.callableName)
        override fun getClassifierNames(): Set<Name> = setOf(typeName)
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

context(_: TypeBehavior, _: FunctionBehavior)
private fun IrPluginContext.referenceAllModuleDependencyGraphs(): List<ModuleDependencyGraph> =
    referenceFunctions(metadataFunctionCallableId).mapNotNull { metadataFunction ->
        metadataFunction
            .owner
            .getAnnotation(metadataAnnotationFqn)
            ?.arguments
            ?.single()
            ?.let { it as? IrConst }
            ?.value
            ?.let { it as? String }
            ?.encodeToByteArray()
            ?.let { metadata ->
                moduleDependencyGraphFromBytes(
                    injectorResolver = { referenceFunctions(FqName(it).asCallableId()).single() },
                    bytes = metadata,
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

private fun ModuleDescriptor.stableOrRegularName(): Name = stableName ?: name

private fun Name.getModuleMetadataFunctionId(): CallableId = CallableId(
    packageName = FqName("${asString().asValidJavaIdentifier()}.__sink_metadata__"),
    className = null,
    callableName = Name.identifier("sinkMetadata"),
)

private val injectableAnnotationFqn = FqName("com.woutwerkman.sink.Injectable")
internal val metadataAnnotationFqn = FqName("com.woutwerkman.sink._SinkMetadata")
private val injectionCacheFqn = FqName("com.woutwerkman.sink.InjectionCache")

internal fun String.asValidJavaIdentifier(): String {
    val first = if (this.first().isJavaIdentifierStart()) this.first() else '_'
    return first + this.drop(1).map { if (it.isJavaIdentifierPart()) it else '_' }.joinToString("")
}

private fun FqName.asCallableId(): CallableId = CallableId(
    packageName = parent(),
    className = null,
    callableName = shortName(),
)

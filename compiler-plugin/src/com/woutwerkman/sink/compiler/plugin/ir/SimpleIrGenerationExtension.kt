package com.woutwerkman.sink.compiler.plugin.ir

// TODO: Ensure Nullability handled
// TODO: Add support for kx Serializer injection? Might be cool icw generic support

import com.woutwerkman.sink.compiler.plugin.metadataFunctionCallableId
import com.woutwerkman.sink.compiler.plugin.somewhatIdentifyingName
import com.woutwerkman.sink.ide.compiler.common.DependencyGraphBuilder
import com.woutwerkman.sink.ide.compiler.common.addFromBytes
import com.woutwerkman.sink.ide.compiler.common.allExternalDependenciesOf
import com.woutwerkman.sink.ide.compiler.common.mapLikely0Or1
import org.jetbrains.kotlin.backend.common.extensions.IrGeneratedDeclarationsRegistrar
import org.jetbrains.kotlin.backend.common.extensions.IrGenerationExtension
import org.jetbrains.kotlin.backend.common.extensions.IrPluginContext
import org.jetbrains.kotlin.backend.common.getCompilerMessageLocation
import org.jetbrains.kotlin.backend.jvm.ir.isInCurrentModule
import org.jetbrains.kotlin.cli.common.messages.CompilerMessageSeverity
import org.jetbrains.kotlin.cli.common.messages.MessageCollector
import org.jetbrains.kotlin.descriptors.ClassKind
import org.jetbrains.kotlin.descriptors.DescriptorVisibilities
import org.jetbrains.kotlin.descriptors.Modality
import org.jetbrains.kotlin.descriptors.ModuleDescriptor
import org.jetbrains.kotlin.descriptors.impl.PackageFragmentDescriptorImpl
import org.jetbrains.kotlin.fir.descriptors.FirModuleDescriptor
import org.jetbrains.kotlin.ir.declarations.*
import org.jetbrains.kotlin.ir.declarations.impl.IrFileImpl
import org.jetbrains.kotlin.ir.expressions.IrConst
import org.jetbrains.kotlin.ir.expressions.IrExpression
import org.jetbrains.kotlin.ir.expressions.IrSpreadElement
import org.jetbrains.kotlin.ir.expressions.IrVararg
import org.jetbrains.kotlin.ir.expressions.impl.IrConstImpl
import org.jetbrains.kotlin.ir.expressions.impl.IrConstructorCallImpl
import org.jetbrains.kotlin.ir.symbols.IrClassifierSymbol
import org.jetbrains.kotlin.ir.symbols.IrFunctionSymbol
import org.jetbrains.kotlin.ir.symbols.impl.IrClassSymbolImpl
import org.jetbrains.kotlin.ir.symbols.impl.IrFileSymbolImpl
import org.jetbrains.kotlin.ir.symbols.impl.IrSimpleFunctionSymbolImpl
import org.jetbrains.kotlin.ir.symbols.impl.IrValueParameterSymbolImpl
import org.jetbrains.kotlin.ir.types.IrSimpleType
import org.jetbrains.kotlin.ir.types.IrType
import org.jetbrains.kotlin.ir.types.IrTypeSystemContextImpl
import org.jetbrains.kotlin.ir.types.classifierOrNull
import org.jetbrains.kotlin.ir.types.typeWith
import org.jetbrains.kotlin.ir.util.*
import org.jetbrains.kotlin.name.CallableId
import org.jetbrains.kotlin.name.ClassId
import org.jetbrains.kotlin.name.FqName
import org.jetbrains.kotlin.name.Name
import org.jetbrains.kotlin.resolve.scopes.MemberScope

class SimpleIrGenerationExtension: IrGenerationExtension {
    override fun generate(moduleFragment: IrModuleFragment, pluginContext: IrPluginContext) {
        val typeBehavior = IrTypeBehavior(IrTypeSystemContextImpl(pluginContext.irBuiltIns))
        context(
            typeBehavior,
            FunctionBehavior(typeBehavior, pluginContext.messageCollector),
            DeclarationContainerBehavior,
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
        declarationContainerBehavior: DeclarationContainerBehavior,
        pluginContext: IrPluginContext,
    )
    private fun generateInternal(moduleFragment: IrModuleFragment) {
        // TODO: Use explicit-API compiler flag for some things?
        val injectableCacheInterface = pluginContext.referenceClass(ClassId.topLevel(injectionCacheFqn))
            ?: return // Early exit, this module does not include Sink compile time dependency
        val injectableCacheType = injectableCacheInterface.typeWith()

        val modulesDependencyGraph = pluginContext.referenceAllModuleDependencyGraphs(
            pluginContext.messageCollector,
            injectableCacheType,
        )

        val topLevelGraph = DependencyGraphBuilder<IrType, IrFunctionSymbol, IrClassifierSymbol, DeclarationContainer>()
            .buildGraph(DeclarationContainer.ModuleAsContainer(moduleFragment), modulesDependencyGraph) { error ->
                when (error) {
                    is AmbiguousDependencyResolution -> pluginContext
                        .messageCollector
                        .reportErrorsAmbiguousResolve(error)
                    is CycleDetected -> pluginContext
                        .messageCollector
                        .reportErrorsForCycle(error.cycle)
                }
            }

        val creationSession = InjectionFunctionCreationSession(
            pluginContext,
            moduleFragment,
            injectableCacheType,
            injectionCacheComputeIfAbsentMethodSymbol = injectableCacheInterface.functions.first().owner.symbol,
        )

        data class InjectableAndInjectionFunction(val injectable: IrFunction, val injectionFunction: IrSimpleFunction)

        val injectablesAndTheirInjectionFunctions = buildList {
            topLevelGraph.forEachInjectable { injectableHostGraph, injectable ->
                add(InjectableAndInjectionFunction(
                    injectable = injectable.owner,
                    injectionFunction = creationSession.generateInjectionFunction(injectable.owner, injectableHostGraph),
                ))
            }
        }

        // Now persist the generated declarations
        injectablesAndTheirInjectionFunctions.forEach { (injectable, injectionFunction) ->
            injectable.file.addChild(injectionFunction)
            metadataDeclarationRegistrar.registerFunctionAsMetadataVisible(injectionFunction)
        }

        topLevelGraph.transformAllGetCalls(moduleFragment, pluginContext, injectableCacheType) { getCall, graph, file ->
            val start = getCall.startOffset
            val end = getCall.endOffset
            val factory = IrFactoryWithSameOffsets(pluginContext.irFactory, pluginContext, start, end)

            val requestedType = getCall.typeArguments.single()!!

            val candidates = graph.findCandidatesForType(requestedType)
            val (_, hostGraph, chosen) = when (candidates.size) {
                0 -> {
                    pluginContext.messageCollector.report(
                        CompilerMessageSeverity.ERROR,
                        "No injection candidates found for type ${requestedType.render()}",
                        getCall.getCompilerMessageLocation(file),
                    )
                    return@transformAllGetCalls getCall
                }
                1 -> candidates.single()
                else -> {
                    pluginContext.messageCollector.report(
                        CompilerMessageSeverity.ERROR,
                        "Found multiple injectable candidates for this type:\n" +
                            candidates.joinToString("\n") { it.value.owner.render() }
                    )
                    return@transformAllGetCalls getCall
                }
            }
            val injectionFunction = creationSession.generateInjectionFunction(chosen.owner, hostGraph)

            val requiredParams: List<IrType> = hostGraph
                .allExternalDependenciesOf(chosen)
                .mapLikely0Or1 { it.type }

            fun IrType.isSubtype(supertype: IrType): Boolean = typeBehavior.isSubtype(this, supertype)
            val arguments = getCall
                .arguments
                .getOrNull(1)
                ?.let { it as? IrVararg }
                ?.elements
                .let { it ?: emptyList() }
                .map { varargElement ->
                    when (varargElement) {
                        is IrSpreadElement -> {
                            pluginContext.messageCollector.report(
                                CompilerMessageSeverity.ERROR,
                                "Cannot use spread operator when passing arguments to InjectionCache.get",
                                varargElement.getCompilerMessageLocation(file),
                            )
                            return@transformAllGetCalls getCall
                        }
                        !is IrExpression -> {
                            pluginContext.messageCollector.report(
                                CompilerMessageSeverity.ERROR,
                                "This is an unexpected argument to `.get`. Please report a bug an include this argument",
                                varargElement.getCompilerMessageLocation(file),
                            )
                            return@transformAllGetCalls getCall
                        }
                        else -> varargElement
                    }
                }
                .also { arguments ->
                    requiredParams.forEachIndexed { i, requiredParam ->
                        val argument = arguments.getOrNull(i) ?: run {
                            pluginContext.messageCollector.report(
                                CompilerMessageSeverity.ERROR,
                                "Missing argument $i, which is an external dependency of ${requestedType.render()}",
                                getCall.getCompilerMessageLocation(file),
                            )
                            return@transformAllGetCalls getCall
                        }
                        if (!argument.type.isSubtype(requiredParam)) {
                            pluginContext.messageCollector.report(
                                CompilerMessageSeverity.ERROR,
                                "Attempted to pass argument of type ${argument.type.render()}, but expected ${requiredParam.render()}",
                                argument.getCompilerMessageLocation(file),
                            )
                            return@transformAllGetCalls getCall
                        }
                    }
                    if (arguments.size > requiredParams.size) {
                        arguments.drop(requiredParams.size).forEach { argument ->
                            pluginContext.messageCollector.report(
                                CompilerMessageSeverity.ERROR,
                                "Too many arguments passed for ${requestedType.render()}",
                                argument.getCompilerMessageLocation(file),
                            )
                        }
                        return@transformAllGetCalls getCall
                    }
                }

            // Build call to the generated injection function
            factory.createCallExpression(
                type = requestedType,
                symbol = injectionFunction.symbol,
                extensionReceiver = getCall.arguments.firstOrNull(),
                arguments = arguments,
                typeArguments = emptyList(),
            )
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
                                    value = topLevelGraph.serializeAsModuleDependencyGraph().decodeToString(),
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

private fun MessageCollector.reportErrorsAmbiguousResolve(error: AmbiguousDependencyResolution) {
    val chain = error.transitiveChain
    if (chain == null) {
        val param = error.requestingFunction.owner.parameters.first { it.name.asString() == error.parameterName }
        report(
            CompilerMessageSeverity.ERROR,
            "Found multiple injectable candidates for this type:\n${
                error.candidateFunctions.joinToString("\n") { it.owner.render() }
            }",
            param.getCompilerMessageLocation(error.requestingFunction.owner.file),
        )
    } else {
        val param = chain.last().let { lastStep ->
            lastStep.requestingFunction.owner.parameters.first { it.name.asString() == lastStep.parameterName }
        }
        report(
            CompilerMessageSeverity.ERROR,
            "Transitively, this dependency depends on ${error.parameterType}, for which there are multiple candidates\n" +
                "Transitive chain: ${chain.reversed().joinToString(" -> ") { it.requestingFunction.owner.returnType.render() }}\n" +
                "Candidates: ${
                    error.candidateFunctions.joinToString("\n") { it.owner.render() }
                }",
            param.getCompilerMessageLocation(error.requestingFunction.owner.file),
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
private fun IrPluginContext.referenceAllModuleDependencyGraphs(
    messageCollector: MessageCollector,
    injectableCacheType: IrSimpleType,
): ModuleDependencyGraph {
    val graph = ModuleDependencyGraph()
    referenceFunctions(metadataFunctionCallableId).forEach { metadataFunction ->
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
                graph.addFromBytes(
                    injectorResolver = {
                        val referenceFunctions = referenceFunctions(FqName(it).asCallableId())
                        when (referenceFunctions.size) {
                            0 -> {
                                messageCollector.report(
                                    CompilerMessageSeverity.WARNING,
                                    "A Sink generated module claimed it provides: $it, but no such function was found",
                                )
                                null
                            }
                            1 -> referenceFunctions.single()
                            else -> {
                                val candidates = referenceFunctions.filter {
                                    it.owner.parameters.firstOrNull { it.kind == IrParameterKind.ExtensionReceiver}?.type == injectableCacheType
                                }
                                when (candidates.size) {
                                    0 -> {
                                        messageCollector.report(
                                            CompilerMessageSeverity.WARNING,
                                            "A Sink generated module claimed it provides: $it, of which there are multiple, but none of them have an extension receiver of type InjectionCache",
                                        )
                                        null
                                    }
                                    1 -> candidates.single()
                                    else -> {
                                        messageCollector.report(
                                            CompilerMessageSeverity.WARNING,
                                            "A Sink generated module claimed it provides: $it, but multiple functions were found:\n" +
                                                "${candidates.joinToString("\n") { it.owner.render() }}\n" +
                                                "Defaulting to: ${candidates.first().owner.render()}",
                                        )
                                        candidates.first()
                                    }
                                }
                            }
                        }
                    },
                    bytes = metadata,
                )
            }
    }
    return graph
}

private fun ModuleDescriptor.stableOrRegularName(): Name = stableName ?: name

private fun Name.getModuleMetadataFunctionId(): CallableId = CallableId(
    packageName = FqName("${asString().asValidJavaIdentifier()}.__sink_metadata__"),
    className = null,
    callableName = Name.identifier("sinkMetadata"),
)

internal val injectableAnnotationFqn = FqName("com.woutwerkman.sink.Injectable")
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

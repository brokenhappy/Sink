package com.woutwerkman.sink.compiler.plugin.ir

import com.woutwerkman.sink.ide.compiler.common.DependencyGraphBuilder
import com.woutwerkman.sink.ide.compiler.common.DependencyGraphFromSources
import com.woutwerkman.sink.ide.compiler.common.FunctionBehavior
import com.woutwerkman.sink.ide.compiler.common.ModuleDependencyGraph
import com.woutwerkman.sink.ide.compiler.common.injectorFunctionNameOf
import org.jetbrains.kotlin.GeneratedDeclarationKey
import org.jetbrains.kotlin.backend.common.extensions.IrPluginContext
import org.jetbrains.kotlin.backend.common.ir.createExtensionReceiver
import org.jetbrains.kotlin.descriptors.DescriptorVisibilities
import org.jetbrains.kotlin.descriptors.DescriptorVisibility
import org.jetbrains.kotlin.descriptors.Modality
import org.jetbrains.kotlin.ir.declarations.*
import org.jetbrains.kotlin.ir.declarations.IrDeclarationOrigin.GeneratedByPlugin
import org.jetbrains.kotlin.ir.expressions.*
import org.jetbrains.kotlin.ir.expressions.impl.IrCallImplWithShape
import org.jetbrains.kotlin.ir.expressions.impl.IrConstImpl
import org.jetbrains.kotlin.ir.expressions.impl.IrFunctionExpressionImpl
import org.jetbrains.kotlin.ir.expressions.impl.IrGetValueImpl
import org.jetbrains.kotlin.ir.symbols.IrClassifierSymbol
import org.jetbrains.kotlin.ir.symbols.IrFunctionSymbol
import org.jetbrains.kotlin.ir.symbols.IrSimpleFunctionSymbol
import org.jetbrains.kotlin.ir.symbols.IrTypeParameterSymbol
import org.jetbrains.kotlin.ir.symbols.impl.IrSimpleFunctionSymbolImpl
import org.jetbrains.kotlin.ir.symbols.impl.IrValueParameterSymbolImpl
import org.jetbrains.kotlin.ir.types.*
import org.jetbrains.kotlin.ir.types.SimpleTypeNullability.MARKED_NULLABLE
import org.jetbrains.kotlin.ir.util.fileOrNull
import org.jetbrains.kotlin.ir.util.fqNameWhenAvailable
import org.jetbrains.kotlin.ir.util.getPackageFragment
import org.jetbrains.kotlin.name.ClassId
import org.jetbrains.kotlin.name.FqName
import org.jetbrains.kotlin.name.Name

internal typealias ModuleDependencyGraph =
    ModuleDependencyGraph<IrFunctionSymbol, IrType, IrClassifierSymbol>
internal typealias DependencyGraph =
    DependencyGraphFromSources<IrFunctionSymbol, IrType, IrClassifierSymbol>
internal typealias ResolvedDependency =
    DependencyGraphBuilder.ResolvedDependency<IrType, IrFunctionSymbol>
internal typealias ExternalDependency =
    DependencyGraphBuilder.ResolvedDependency.ExternalDependency<IrType, IrFunctionSymbol>
internal typealias ImplementationDetail =
    DependencyGraphBuilder.ResolvedDependency.ImplementationDetail<IrType, IrFunctionSymbol>
internal typealias TypeBehavior =
    com.woutwerkman.sink.ide.compiler.common.TypeBehavior<IrType, IrClassifierSymbol, IrTypeParameterSymbol>
internal typealias FunctionBehavior =
    FunctionBehavior<IrType, IrFunctionSymbol>

internal class InjectionFunctionCreationSession(
    private val pluginContext: IrPluginContext,
    private val moduleFragment: IrModuleFragment,
    private val injectionCacheType: IrSimpleType,
    private val typeBehavior: TypeBehavior,
    private val injectionCacheComputeIfAbsentMethodSymbol: IrSimpleFunctionSymbol,
) {
    private val cache = mutableMapOf<IrFunction, InjectionFunction>()

    private data class InjectionFunction(
        val functionSymbol: IrSimpleFunctionSymbol,
        val declaration: IrSimpleFunction,
    )

    internal fun generateInjectionFunction(instantiator: IrFunction, graph: DependencyGraph): IrSimpleFunction =
        generateInjectionFunctionInternal(instantiator, graph).declaration!!

    private fun generateInjectionFunctionInternal(
        instantiator: IrFunction,
        graph: DependencyGraph,
    ): InjectionFunction = cache.getOrPut(instantiator) {
        if (!instantiator.isDeclaredIn(moduleFragment)) return@getOrPut InjectionFunction(
            declaration = instantiator as IrSimpleFunction,
            functionSymbol = instantiator.symbol,
        )

        val factory = IrFactoryWithSameOffsets(
            factory = pluginContext.irFactory,
            pluginContext = pluginContext,
            startOffset = instantiator.startOffset,
            endOffset = instantiator.endOffset,
        )
        val symbol = IrSimpleFunctionSymbolImpl()

        InjectionFunction(
            functionSymbol = symbol,
            declaration = factory.createSimpleExpressionBodyFunction( /** Represents: [_DocRefFunctionDeclaration] */
                name = Name.identifier(typeBehavior.injectorFunctionNameOf(instantiator.returnType)),
                receiver = injectionCacheType,
                returnType = instantiator.returnType,
                origin = GeneratedByPlugin(SinkPluginKey),
                visibility = instantiator
                    .returnType
                    .getClassIds()
                    .map { (pluginContext.referenceClass(it) ?: error("Class does not exist $it")).owner.visibility }
                    .sortedWith { a, b -> a.compareTo(b) ?: Int.MAX_VALUE }
                    .last(), // TODO: Handle use case: Instantiator inside of private class
                symbol = symbol,
                parameters = graph.allExternalDependenciesOf(instantiator.symbol).map { dependency ->
                    pluginContext.irFactory.createValueParameter(/** Represents: [_DocRefExternalDependencyParameter] */
                        startOffset = instantiator.startOffset,
                        endOffset = instantiator.endOffset,
                        origin = GeneratedByPlugin(SinkPluginKey), // TODO: Better?
                        name = Name.identifier(dependency.parameterName),
                        type = dependency.type,
                        symbol = IrValueParameterSymbolImpl(),
                        isAssignable = false,
                        varargElementType = null,
                        isCrossinline = false,
                        isNoinline = false,
                        isHidden = false,
                        kind = IrParameterKind.Regular,
                    )
                },
                expressionCreator = { injectionCacheReceiverParameterCreator, functionDeclaration ->
                    factory.createCallExpression( /** Represents: [_DocRefComputeIfAbsent] */
                        type = instantiator.returnType,
                        symbol = injectionCacheComputeIfAbsentMethodSymbol,
                        dispatchReceiver = injectionCacheReceiverParameterCreator()!!,
                        typeArguments = listOf(instantiator.returnType),
                        arguments = listOf(
                            factory.stringConstant(instantiator.returnType.toCacheKey()), /** Represents: [_DocRefKey] */
                            factory.noArgLambda( /** Represents: [_DocRefLambdaArgument] */
                                type = instantiator.returnType,
                                lambdaOwnerFunction = functionDeclaration,
                                expression = factory.createCallExpression( /** Represents: [_DocRefInstantiatorCall] */
                                    type = instantiator.returnType,
                                    symbol = instantiator.symbol as IrSimpleFunctionSymbol,
                                    arguments = graph.instantiatorFunctionsToDependencies[instantiator.symbol]!!
                                        .map { dependency ->
                                            createDependencyProvidingArgument(
                                                dependency,
                                                graph,
                                                factory,
                                                injectionCacheReceiverParameterCreator,
                                                getParameterValueByType = { type ->
                                                    IrGetValueImpl(
                                                        startOffset = instantiator.startOffset,
                                                        endOffset = instantiator.endOffset,
                                                        type = type,
                                                        symbol = functionDeclaration.parameters.first { it.type == type }.symbol,
                                                        origin = GeneratedBySink,
                                                    )
                                                }
                                            )
                                        },
                                ),
                            ),
                        ),
                    )
                },
                // TODO: Handle generics
            )
        )
    }

    private fun createDependencyProvidingArgument(
        dependency: ResolvedDependency,
        graph: DependencyGraph,
        factory: IrFactoryWithSameOffsets,
        injectionCacheReceiverParameterCreator: () -> IrGetValue?,
        getParameterValueByType: (IrType) -> IrGetValue,
    ): IrExpression = when (dependency) {
        is ExternalDependency ->
            getParameterValueByType(dependency.type) /** Represents: [_DocRefExternalDependencyDrill] */
        is ImplementationDetail ->
            factory.createCallExpression( /** Represents: [_DocRefInjectorCall] */
                type = dependency.instantiatorOrInjectorFunction.owner.returnType,
                symbol = generateInjectionFunctionInternal(
                    dependency.instantiatorOrInjectorFunction.owner,
                    graph,
                ).functionSymbol,
                extensionReceiver = injectionCacheReceiverParameterCreator(), /** Represents: [_DocRefThisValue] */
                arguments = graph
                    .allExternalDependenciesOf(dependency.instantiatorOrInjectorFunction)
                    .map { subDependency ->
                        createDependencyProvidingArgument(
                            subDependency,
                            graph,
                            factory,
                            injectionCacheReceiverParameterCreator,
                            getParameterValueByType,
                        )
                    }
                    // TODO: Support generics
            )
    }
}


/**
 * Given:
 * Module A:
 * ```kt
 * package modulea
 * @Injectable
 * fun foo(): Foo = TODO()
 * fun bar(foo: Foo, baz: Baz, buz: Buz): Bar = TODO()
 * ```
 *
 * Module B(A):
 * ```kt
 * package moduleb
 *
 * interface Barbs
 *
 * @Injectable
 * fun baz(foo: Foo, bazbs: Bazbs): Baz = TODO()
 * @Injectable
 * fun foobs(foo: Foo): Foobs = TODO()
 * @Injectable
 * fun barbs(bar: Bar, foobs: Foobs, bazbs: Bazbs): Barbs = TODO()
 * ```
 *
 * For `barbs` it will generate:
 */

private typealias _DocRefFunctionDeclaration = Nothing // -------> fun InjectionCache.Barbs(
private typealias _DocRefExternalDependencyParameter = Nothing //>   bazbs: Bazbs,
/** Same as      [_DocRefExternalDependencyParameter] */ // ----->   buz: Buz,
private typealias _DocRefThisValue = Nothing // -----------------> ) = this
private typealias _DocRefComputeIfAbsent = Nothing // ----------->   .computeIfAbsent(
private typealias _DocRefKey = Nothing // ----------------------->     key = "moduleb.Barbs"
private typealias _DocRefLambdaArgument = Nothing // ------------>     compute = {
private typealias _DocRefInstantiatorCall = Nothing // ---------->       barbs(
private typealias _DocRefInjectorCall = Nothing // -------------->         bar = this.Bar(
/** Same as      [_DocRefInjectorCall] */ // -------------------->           baz = this.Baz(
private typealias _DocRefExternalDependencyDrill = Nothing // --->             bazbs = bazbs,
                                                                //           ),
/** Same as      [_DocRefExternalDependencyDrill] */ // --------->           buz = buz,
                                                                //         )
/** Same as      [_DocRefInjectorCall] */ // -------------------->         foobs = this.Foobs(),
/** Same as      [_DocRefExternalDependencyDrill] */ // --------->         bazbs = bazbs,
                                                                //       )
                                                                //     }
                                                                //   )

private val GeneratedBySink by IrStatementOriginImpl

private fun IrDeclaration.isDeclaredIn(module: IrModuleFragment): Boolean = fileOrNull?.module == module

internal class IrFactoryWithSameOffsets(
    val factory: IrFactory,
    val pluginContext: IrPluginContext,
    val startOffset: Int,
    val endOffset: Int,
) {
    fun createSimpleExpressionBodyFunction(
        name: Name,
        visibility: DescriptorVisibility,
        returnType: IrType,
        origin: IrDeclarationOrigin,
        symbol: IrSimpleFunctionSymbol,
        expressionCreator: (
            receiverValueParameterExpressionCreator: () -> IrGetValue?,
            functionDeclaration: IrSimpleFunction,
        ) -> IrExpression,
        parameters: List<IrValueParameter>,
        receiver: IrType?,
        isInline: Boolean = false,
        isExpect: Boolean = false,
        modality: Modality = Modality.FINAL,
        isTailrec: Boolean = false,
        isSuspend: Boolean = false,
        isOperator: Boolean = false,
        isInfix: Boolean = false,
        isExternal: Boolean = false,
    ): IrSimpleFunction = factory.createSimpleFunction(
        startOffset = startOffset,
        endOffset = endOffset,
        origin = origin,
        isInline = isInline,
        isExpect = isExpect,
        modality = modality,
        isTailrec = isTailrec,
        isSuspend = isSuspend,
        isOperator = isOperator,
        isInfix = isInfix,
        isExternal = isExternal,
        name = name,
        visibility = visibility,
        returnType = returnType,
        symbol = symbol,
    ).also { function ->
        val receiverValueParameter = receiver?.let { function.createExtensionReceiver(receiver) }

        (listOfNotNull(receiverValueParameter) + parameters).forEach {
            function.parameters += it
            it.parent = function
        }

        function.body = factory.createExpressionBody(
            startOffset,
            endOffset,
            expressionCreator(
                /* receiverValueParameterExpressionCreator = */ {
                    receiver?.let {
                        IrGetValueImpl(
                            startOffset,
                            endOffset,
                            type = receiver,
                            origin = GeneratedBySink,
                            symbol = receiverValueParameter!!.symbol
                        )
                    }
                },
                /* functionDeclaration = */ function,
            ),
        )
    }

    fun createCallExpression(
        type: IrType,
        symbol: IrSimpleFunctionSymbol,
        extensionReceiver: IrExpression? = null,
        dispatchReceiver: IrExpression? = null,
        arguments: List<IrExpression>,
        argumentsCount: Int = arguments.size,
        typeArguments: List<IrType> = emptyList(),
    ): IrCall = IrCallImplWithShape(
        startOffset = startOffset,
        endOffset = endOffset,
        type = type,
        symbol = symbol,
        typeArgumentsCount = typeArguments.size,
        valueArgumentsCount = argumentsCount,
        origin = GeneratedBySink,
        superQualifierSymbol = null,
        contextParameterCount = 0,
        hasDispatchReceiver = dispatchReceiver != null,
        hasExtensionReceiver = extensionReceiver != null,
    ).also { call ->
        (listOfNotNull(dispatchReceiver, extensionReceiver) + arguments).forEachIndexed { index, argument ->
            call.arguments[index] = argument
        }
        typeArguments.forEachIndexed { index, typeArgument ->
            call.typeArguments[index] = typeArgument
        }
    }

    fun stringConstant(value: String): IrConst =
        IrConstImpl.string(startOffset, endOffset, pluginContext.irBuiltIns.stringType, value)

    fun noArgLambda(
        type: IrType,
        expression: IrExpression,
        lambdaOwnerFunction: IrSimpleFunction,
    ): IrFunctionExpression = IrFunctionExpressionImpl(
        startOffset = startOffset,
        endOffset = endOffset,
        origin = IrStatementOrigin.LAMBDA,
        type = pluginContext.irBuiltIns.functionN(0).typeWith(type),
        function = createSimpleExpressionBodyFunction(
            origin = IrDeclarationOrigin.LOCAL_FUNCTION_FOR_LAMBDA,
            name = Name.identifier("<anonymous>"),
            visibility = DescriptorVisibilities.LOCAL,
            returnType = type,
            symbol = IrSimpleFunctionSymbolImpl(),
            receiver = null,
            parameters = emptyList(),
            expressionCreator = { _, _, -> expression },
        ).apply {
            parent = lambdaOwnerFunction
        },
    )
}

private fun IrType.getClassIds(): List<ClassId> = buildList {
    fun IrType.visit() {
        when (this) {
            is IrDynamicType -> TODO()
            is IrErrorType -> TODO()
            is IrSimpleType -> {
                add(ClassId(
                    packageFqName = classifier.owner.getPackageFragment()?.packageFqName ?: return,
                    relativeClassName = (classifier.owner as? IrDeclarationWithName)?.fqNameWhenAvailable?.shortName()?.asString()?.let(::FqName) ?: return,
                    isLocal = false,
                ))
                arguments.forEach { it.typeOrNull?.visit() }
            }
        }
    }
    this@getClassIds.visit()
}

/** This key is used to identify the injectable in the injection cache */
internal fun IrType.toCacheKey(): String = when (this) {
    is IrDynamicType -> TODO()
    is IrErrorType -> TODO()
    is IrSimpleType -> ((classifier.owner as? IrDeclarationWithName)?.fqNameWhenAvailable?.asString() ?: "Unknown") +
        typeArgumentsToString(separator = ",", prefix = "<", suffix = ">", typeToString = IrType::toCacheKey) +
        if (nullability == MARKED_NULLABLE) "?" else ""
}

internal fun IrSimpleType.typeArgumentsToString(
    separator: String,
    prefix: String,
    suffix: String,
    typeToString: (IrType) -> String,
): String =
    if (arguments.isEmpty()) ""
    else arguments.joinToString(separator = separator, prefix = prefix, postfix = suffix) {
        it.typeOrNull?.let(typeToString) ?: "Unknown"
    }

internal object SinkPluginKey: GeneratedDeclarationKey()

private fun DependencyGraph.allExternalDependenciesOf(function: IrFunctionSymbol): List<ExternalDependency> =
    instantiatorFunctionsToDependencies[function]?.allExternalDependencies() ?: emptyList()

private fun List<ResolvedDependency>.allExternalDependencies(): List<ExternalDependency> = flatMap { dependency ->
    when (dependency) {
        is ImplementationDetail -> dependency.indirectDependencies.allExternalDependencies()
        is ExternalDependency -> listOf(dependency)
    }
}
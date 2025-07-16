package org.jetbrains.kotlin.compiler.plugin.template.ir

import org.jetbrains.kotlin.GeneratedDeclarationKey
import org.jetbrains.kotlin.backend.common.extensions.IrPluginContext
import org.jetbrains.kotlin.backend.common.ir.createExtensionReceiver
import org.jetbrains.kotlin.descriptors.DescriptorVisibilities
import org.jetbrains.kotlin.descriptors.DescriptorVisibility
import org.jetbrains.kotlin.descriptors.Modality
import org.jetbrains.kotlin.ir.builders.declarations.addValueParameter
import org.jetbrains.kotlin.ir.declarations.IrDeclarationOrigin
import org.jetbrains.kotlin.ir.declarations.IrDeclarationOrigin.*
import org.jetbrains.kotlin.ir.declarations.IrDeclarationWithName
import org.jetbrains.kotlin.ir.declarations.IrFactory
import org.jetbrains.kotlin.ir.declarations.IrFunction
import org.jetbrains.kotlin.ir.declarations.IrModuleFragment
import org.jetbrains.kotlin.ir.declarations.IrSimpleFunction
import org.jetbrains.kotlin.ir.declarations.IrValueParameter
import org.jetbrains.kotlin.ir.expressions.IrCall
import org.jetbrains.kotlin.ir.expressions.IrConst
import org.jetbrains.kotlin.ir.expressions.IrExpression
import org.jetbrains.kotlin.ir.expressions.IrFunctionExpression
import org.jetbrains.kotlin.ir.expressions.IrGetValue
import org.jetbrains.kotlin.ir.expressions.IrStatementOrigin
import org.jetbrains.kotlin.ir.expressions.IrStatementOriginImpl
import org.jetbrains.kotlin.ir.expressions.impl.IrCallImplWithShape
import org.jetbrains.kotlin.ir.expressions.impl.IrConstImpl
import org.jetbrains.kotlin.ir.expressions.impl.IrFunctionExpressionImpl
import org.jetbrains.kotlin.ir.expressions.impl.IrGetValueImpl
import org.jetbrains.kotlin.ir.expressions.impl.IrReturnImpl
import org.jetbrains.kotlin.ir.symbols.IrSimpleFunctionSymbol
import org.jetbrains.kotlin.ir.symbols.impl.IrSimpleFunctionSymbolImpl
import org.jetbrains.kotlin.ir.symbols.impl.IrValueParameterSymbolImpl
import org.jetbrains.kotlin.ir.types.IrDynamicType
import org.jetbrains.kotlin.ir.types.IrErrorType
import org.jetbrains.kotlin.ir.types.IrSimpleType
import org.jetbrains.kotlin.ir.types.IrType
import org.jetbrains.kotlin.ir.types.SimpleTypeNullability.MARKED_NULLABLE
import org.jetbrains.kotlin.ir.types.typeOrNull
import org.jetbrains.kotlin.ir.types.typeWith
import org.jetbrains.kotlin.ir.util.fqNameWhenAvailable
import org.jetbrains.kotlin.ir.util.getPackageFragment
import org.jetbrains.kotlin.name.CallableId
import org.jetbrains.kotlin.name.ClassId
import org.jetbrains.kotlin.name.FqName
import org.jetbrains.kotlin.name.Name
import java.util.*

internal fun Map<IrFunction, List<ResolvedDependency>>.findCycles(): List<List<IrFunction>> {
    val visited = mutableSetOf<IrFunction>()
    val recursionStack = mutableSetOf<IrFunction>()
    val cycles = mutableListOf<List<IrFunction>>()
    val currentPath = LinkedList<IrFunction>()

    fun dfs(node: IrFunction) {
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
                is ResolvedDependency.MatchFound -> dfs(next.instantiatorFunction)
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

internal class InjectionFunctionCreationSession(
    private val pluginContext: IrPluginContext,
    private val moduleFragment: IrModuleFragment,
    private val injectionCacheType: IrSimpleType,
    private val injectionCacheComputeIfAbsentMethodSymbol: IrSimpleFunctionSymbol,
) {
    private val cache = mutableMapOf<IrType, InjectionFunction>()

    private data class InjectionFunction(
        val functionSymbol: IrSimpleFunctionSymbol,
        /** null if it's injectable is declared outside [moduleFragment] */
        val declaration: IrSimpleFunction?,
    )

    internal fun generateInjectionFunction(
        instantiator: IrFunction,
        graph: GraphBuildResult.NoIssues,
    ): IrSimpleFunction = generateInjectionFunctionInternal(instantiator, graph).declaration!!

    private fun generateInjectionFunctionInternal(
        instantiator: IrFunction,
        graph: GraphBuildResult.NoIssues,
    ): InjectionFunction = cache.computeIfAbsent(instantiator.returnType) { _ ->
        if (!instantiator.isDeclaredIn(moduleFragment)) return@computeIfAbsent InjectionFunction(
            functionSymbol = pluginContext.referenceFunctions(CallableId(
                packageName = instantiator.getPackageFragment().packageFqName,
                className = null,
                callableName = Name.identifier(instantiator.returnType.asFunctionName()),
            )).single(), // TODO: Handle non single
            declaration = null,
        )

        val factory = IrFactoryWithSameOffsets(
            factory = pluginContext.irFactory,
            pluginContext = pluginContext,
            startOffset = instantiator.startOffset,
            endOffset = instantiator.endOffset,
        )
        val symbol = IrSimpleFunctionSymbolImpl()
        val dependencies = graph.graph[instantiator]!!
        val missingDependencies = dependencies.mapNotNull { dependency ->
            when (dependency) {
                is ResolvedDependency.MatchFound -> null
                is ResolvedDependency.MissingDependency ->
                    pluginContext.irFactory.createValueParameter( /** Represents: [_DocRefMissingDependencyParameter] */
                        startOffset = instantiator.startOffset,
                        endOffset = instantiator.endOffset,
                        origin = GeneratedByPlugin(SinkPluginKey), // TODO: Better?
                        name = dependency.parameterName,
                        type = dependency.type,
                        symbol = IrValueParameterSymbolImpl(),
                        isAssignable = false,
                        varargElementType = null,
                        isCrossinline = false,
                        isNoinline = false,
                        isHidden = false,
                    )
            }
        }

        InjectionFunction(
            functionSymbol = symbol,
            declaration = factory.createSimpleExpressionBodyFunction( /** Represents: [_DocRefFunctionDeclaration] */
                name = Name.identifier(instantiator.returnType.asFunctionName()),
                receiver = injectionCacheType,
                returnType = instantiator.returnType,
                origin = GeneratedByPlugin(SinkPluginKey),
                visibility = instantiator
                    .returnType
                    .getClassIds()
                    .map {
                        (pluginContext.referenceClass(it) ?: error("Class does not exist $it")).owner.visibility
                    }
                    .sortedWith { a, b -> a.compareTo(b) ?: Int.MAX_VALUE }
                    .last(), // TODO: Handle use case: Instantiator inside of private class
                symbol = symbol,
                parameters = missingDependencies,
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
                                    arguments = dependencies.map { dependency ->
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
                                                    symbol = missingDependencies.first { it.type == type }.symbol,
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
        graph: GraphBuildResult.NoIssues,
        factory: IrFactoryWithSameOffsets,
        injectionCacheReceiverParameterCreator: () -> IrGetValue?,
        getParameterValueByType: (IrType) -> IrGetValue,
    ): IrExpression = when (dependency) {
        is ResolvedDependency.MissingDependency ->
            getParameterValueByType(dependency.type) /** Represents: [_DocRefMissingDependencyDrill] */
        is ResolvedDependency.MatchFound ->
            factory.createCallExpression( /** Represents: [_DocRefInjectorCall] */
                type = dependency.instantiatorFunction.returnType,
                symbol = generateInjectionFunctionInternal(
                    dependency.instantiatorFunction,
                    graph,
                ).functionSymbol,
                extensionReceiver = injectionCacheReceiverParameterCreator(), /** Represents: [_DocRefThisValue] */
                arguments = graph
                    .graph[dependency.instantiatorFunction]
                    ?.map { subDependency ->
                        createDependencyProvidingArgument(
                            subDependency,
                            graph,
                            factory,
                            injectionCacheReceiverParameterCreator,
                            getParameterValueByType,
                        )
                    } ?: emptyList(),
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
 * Module B:
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

private typealias _DocRefFunctionDeclaration = Nothing // ------> fun InjectionCache.Barbs(
private typealias _DocRefMissingDependencyParameter = Nothing //>   bazbs: Bazbs,
/** Same as      [_DocRefMissingDependencyParameter] */ // ----->   buz: Buz,
private typealias _DocRefThisValue = Nothing // ----------------> ) = this
private typealias _DocRefComputeIfAbsent = Nothing // ---------->   .computeIfAbsent(
private typealias _DocRefKey = Nothing // ---------------------->     key = "moduleb.Barbs"
private typealias _DocRefLambdaArgument = Nothing // ----------->     compute = {
private typealias _DocRefInstantiatorCall = Nothing // --------->       barbs(
private typealias _DocRefInjectorCall = Nothing // ------------->         bar = this.Bar(
/** Same as      [_DocRefInjectorCall] */ // ------------------->           baz = this.Baz(
private typealias _DocRefMissingDependencyDrill = Nothing // --->             bazbs = bazbs,
                                                               //           ),
/** Same as      [_DocRefMissingDependencyDrill] */ // --------->           buz = buz, // TODO: Do we need to handle missing dependencies recursively?
                                                               //         )
/** Same as      [_DocRefInjectorCall] */ // ------------------->         foobs = this.Foobs(),
/** Same as      [_DocRefMissingDependencyDrill] */ // --------->         bazbs = bazbs,
                                                               //       )
                                                               //     }
                                                               //   )

private val GeneratedBySink by IrStatementOriginImpl

private class IrFactoryWithSameOffsets(
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
        function.extensionReceiverParameter = receiverValueParameter
        (listOfNotNull(receiverValueParameter) + parameters).forEach {
            function.addValueParameter(it.name, it.type, it.origin)
        }

        function.body = factory.createBlockBody(startOffset, endOffset).also { body ->
            body.statements.add(IrReturnImpl(
                startOffset,
                endOffset,
                returnType,
                symbol,
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
                )
            ))
        }
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
        valueArgumentsCount = argumentsCount + (if (extensionReceiver != null) 1 else 0), // + (if (dispatchReceiver != null) 1 else 0),
        origin = GeneratedBySink,
        superQualifierSymbol = null,
        contextParameterCount = 0,
        hasDispatchReceiver = dispatchReceiver != null,
        hasExtensionReceiver = extensionReceiver != null,
    ).also { call ->
        call.extensionReceiver = extensionReceiver
        call.dispatchReceiver = dispatchReceiver
        (listOfNotNull(extensionReceiver, dispatchReceiver) + arguments).forEachIndexed { index, argument ->
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


private fun IrType.asFunctionName(): String = when (this) {
    is IrDynamicType -> TODO()
    is IrErrorType -> TODO()
    is IrSimpleType -> (this.classifier.owner as IrDeclarationWithName).name.asStringStripSpecialMarkers() +
        typeArgumentsToString(separator = "And", prefix = "Of", suffix = "") { it.asFunctionName() } +
        if (nullability == MARKED_NULLABLE) "OrNull" else ""
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
private fun IrType.toCacheKey(): String = when (this) {
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

object SinkPluginKey: GeneratedDeclarationKey()

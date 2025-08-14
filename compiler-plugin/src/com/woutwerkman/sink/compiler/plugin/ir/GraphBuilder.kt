package com.woutwerkman.sink.compiler.plugin.ir

import com.woutwerkman.sink.ide.compiler.common.ConcreteType
import com.woutwerkman.sink.ide.compiler.common.DeclarationVisibility
import com.woutwerkman.sink.ide.compiler.common.TypeVariance
import com.woutwerkman.sink.ide.compiler.common.WithVariance
import org.jetbrains.kotlin.cli.common.messages.CompilerMessageSeverity
import org.jetbrains.kotlin.cli.common.messages.MessageCollector
import org.jetbrains.kotlin.descriptors.DescriptorVisibilities
import org.jetbrains.kotlin.descriptors.DescriptorVisibility
import org.jetbrains.kotlin.ir.declarations.IrClass
import org.jetbrains.kotlin.ir.declarations.IrDeclarationWithVisibility
import com.woutwerkman.sink.ide.compiler.common.mapNotNullLikely0Or1
import org.jetbrains.kotlin.ir.declarations.IrConstructor
import org.jetbrains.kotlin.ir.declarations.IrFunction
import org.jetbrains.kotlin.ir.expressions.IrConst
import org.jetbrains.kotlin.ir.symbols.IrClassifierSymbol
import org.jetbrains.kotlin.ir.symbols.IrFunctionSymbol
import org.jetbrains.kotlin.ir.symbols.IrTypeParameterSymbol
import org.jetbrains.kotlin.ir.types.IrSimpleType
import org.jetbrains.kotlin.ir.types.IrType
import org.jetbrains.kotlin.ir.types.IrTypeSystemContext
import org.jetbrains.kotlin.ir.types.typeOrNull
import org.jetbrains.kotlin.ir.util.fqNameWhenAvailable
import org.jetbrains.kotlin.ir.util.getAnnotation
import org.jetbrains.kotlin.ir.util.hasAnnotation
import org.jetbrains.kotlin.ir.util.isSubtypeOf
import org.jetbrains.kotlin.ir.util.parentAsClass
import org.jetbrains.kotlin.ir.util.superTypes


internal class FunctionBehavior(
    private val typeBehavior: IrTypeBehavior,
    private val messageCollector: MessageCollector,
) : com.woutwerkman.sink.ide.compiler.common.FunctionBehavior<IrType, IrFunctionSymbol> {
    override fun getReturnTypeOf(injectable: IrFunctionSymbol): IrType = injectable.owner.returnType
    override fun getFqnOf(injectable: IrFunctionSymbol): String =
        injectable.owner.fqNameWhenAvailable!!.asString()

    override fun getVisibilityOf(injectable: IrFunctionSymbol): DeclarationVisibility = injectable
        .owner
        .getAnnotation(injectableAnnotationFqn)
        ?.arguments
        ?.let { arguments ->
            when (val argument = arguments.firstOrNull()?.let { it as? IrConst }?.value as? String) {
                null -> typeBehavior.getMinimumVisibilityOf(getReturnTypeOf(injectable))
                "public" -> DeclarationVisibility.Public
                "internal" -> DeclarationVisibility.Internal
                "private" -> DeclarationVisibility.Private
                else -> {
                    messageCollector.report(
                        CompilerMessageSeverity.ERROR,
                        "Visibility must be one of: public, internal, or private. Instead got: $argument",
                    )
                    typeBehavior.getMinimumVisibilityOf(getReturnTypeOf(injectable))
                }
            }
        }
        ?: injectable.owner.visibility.toSinkVisibility()

    override fun getParametersOf(injectable: IrFunctionSymbol): List<Pair<String, IrType>> =
        injectable.owner.parameters.map { it.name.asString() to it.type }
}

internal object DeclarationContainerBehavior: com.woutwerkman.sink.ide.compiler.common.DeclarationContainerBehavior<DeclarationContainer, IrFunctionSymbol> {
    override fun getChildrenOf(container: DeclarationContainer): List<DeclarationContainer> {
        when (container) {
            is DeclarationContainer.FileAsContainer -> TODO()
            is DeclarationContainer.ModuleAsContainer -> TODO()
            is DeclarationContainer.ObjectAsContainer -> TODO()
        }
    }

    override fun getDeclarationsOf(container: DeclarationContainer): List<IrFunctionSymbol> = when (container) {
        is DeclarationContainer.FileAsContainer -> container.file.declarations
        is DeclarationContainer.ModuleAsContainer -> emptyList()
        is DeclarationContainer.ObjectAsContainer -> container.objectDeclaration.declarations
    }.mapNotNullLikely0Or1 { (it as? IrFunction)?.takeIf { it.declaresInjectable() }?.symbol }

    override fun getVisibilityOf(container: DeclarationContainer): DeclarationVisibility = when (container) {
        is DeclarationContainer.FileAsContainer -> DeclarationVisibility.Public
        is DeclarationContainer.ModuleAsContainer -> DeclarationVisibility.Public
        is DeclarationContainer.ObjectAsContainer -> container.objectDeclaration.visibility.toSinkVisibility()
    }

    private fun IrFunction.declaresInjectable(): Boolean = hasAnnotation(injectableAnnotationFqn) ||
        (this is IrConstructor && this.parentAsClass.hasAnnotation(injectableAnnotationFqn))
}


internal class IrTypeBehavior(val typeSystemContext: IrTypeSystemContext) : TypeBehavior {
    override fun getFqnOf(symbol: IrClassifierSymbol): String =
        (symbol.owner as? IrClass)?.fqNameWhenAvailable?.asString() ?: "UNKNOWN"

    override fun getMinimumVisibilityOf(expression: IrType): DeclarationVisibility =
        getMinimumDescriptorVisibilityOf(expression).toSinkVisibility()

    /** Null means visibility is unknown */
    private fun getMinimumDescriptorVisibilityOf(expression: IrType): DescriptorVisibility? {
        val (classifier, arguments) = asConcreteType(expression) ?: return DescriptorVisibilities.PUBLIC
        val visibility = (classifier as? IrDeclarationWithVisibility)?.visibility ?: return null
        return arguments.fold(visibility) { acc, it ->
            getMinimumDescriptorVisibilityOf(it)?.coerceAtLeast(acc) ?: return null
        }
    }

    override fun isSubtype(subtype: IrType, supertype: IrType): Boolean =
        subtype.isSubtypeOf(supertype, typeSystemContext)

    override fun getVarianceOf(typeParameter: IrTypeParameterSymbol): TypeVariance {
        TODO("Not yet implemented")
    }

    override fun asConcreteType(type: IrType): ConcreteType<IrClassifierSymbol, IrType>? = (type as? IrSimpleType)
        ?.let { type -> ConcreteType(type.classifier, type.arguments.mapNotNull { it.typeOrNull }) }

    override fun superTypesOfWithoutAny(symbol: IrClassifierSymbol): Sequence<IrType> =
        symbol.superTypes().asSequence()

    override fun getTypeParameterSymbolsOf(symbol: IrClassifierSymbol): List<IrTypeParameterSymbol> {
        TODO("Not yet implemented")
    }

    override fun isStarProjection(type: IrType): Boolean {
        TODO("Not yet implemented")
    }

    override fun unwrapNullableOrNull(type: IrType): IrType? {
        TODO("Not yet implemented")
    }

    override fun isTopType(type: IrType): Boolean {
        TODO("Not yet implemented")
    }

    override fun isBottomType(type: IrType): Boolean {
        TODO("Not yet implemented")
    }

    override fun asTypeParameterReference(type: IrType): IrTypeParameterSymbol? {
        TODO("Not yet implemented")
    }

    override fun unwrapVarianceOrNull(type: IrType): WithVariance<IrType>? {
        TODO("Not yet implemented")
    }

}

private fun DescriptorVisibility?.toSinkVisibility(): DeclarationVisibility =
    when (this) {
        DescriptorVisibilities.PUBLIC -> DeclarationVisibility.Public
        DescriptorVisibilities.INTERNAL -> DeclarationVisibility.Internal
        else -> DeclarationVisibility.Private
    }

private fun DescriptorVisibility.coerceAtLeast(minimumValue: DescriptorVisibility): DescriptorVisibility? =
    this.compareTo(minimumValue)?.let { comparisonResult -> if (comparisonResult < 0) minimumValue else this }

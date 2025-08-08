package com.woutwerkman.sink.compiler.plugin.ir

import com.woutwerkman.sink.ide.compiler.common.ConcreteType
import com.woutwerkman.sink.ide.compiler.common.DeclarationVisibility
import com.woutwerkman.sink.ide.compiler.common.TypeVariance
import com.woutwerkman.sink.ide.compiler.common.WithVariance
import org.jetbrains.kotlin.descriptors.DescriptorVisibilities
import org.jetbrains.kotlin.descriptors.DescriptorVisibility
import org.jetbrains.kotlin.ir.declarations.IrClass
import org.jetbrains.kotlin.ir.declarations.IrDeclarationWithVisibility
import org.jetbrains.kotlin.ir.symbols.IrClassifierSymbol
import org.jetbrains.kotlin.ir.symbols.IrFunctionSymbol
import org.jetbrains.kotlin.ir.symbols.IrTypeParameterSymbol
import org.jetbrains.kotlin.ir.types.IrSimpleType
import org.jetbrains.kotlin.ir.types.IrType
import org.jetbrains.kotlin.ir.types.IrTypeSystemContext
import org.jetbrains.kotlin.ir.types.typeOrNull
import org.jetbrains.kotlin.ir.util.fileOrNull
import org.jetbrains.kotlin.ir.util.fqNameWhenAvailable
import org.jetbrains.kotlin.ir.util.isSubtypeOf
import org.jetbrains.kotlin.ir.util.superTypes


internal val functionBehavior = object : FunctionBehavior {
    override fun getReturnTypeOf(injectable: IrFunctionSymbol): IrType = injectable.owner.returnType
    override fun getModuleOf(injectable: IrFunctionSymbol): Any? = injectable.owner.fileOrNull?.module
    override fun getFqnOf(injectable: IrFunctionSymbol): String =
        injectable.owner.fqNameWhenAvailable!!.asString()

    override fun getParametersOf(injectable: IrFunctionSymbol): List<Pair<String, IrType>> =
        injectable.owner.parameters.map { it.name.asString() to it.type }
}

internal class IrTypeBehavior(val typeSystemContext: IrTypeSystemContext) : TypeBehavior {
    override fun getFqnOf(symbol: IrClassifierSymbol): String =
        (symbol.owner as? IrClass)?.fqNameWhenAvailable?.asString() ?: "UNKNOWN"

    override fun getMinimumVisibilityOf(expression: IrType): DeclarationVisibility =
        when (getMinimumDescriptorVisibilityOf(expression)) {
            DescriptorVisibilities.PUBLIC -> DeclarationVisibility.Public
            DescriptorVisibilities.INTERNAL -> DeclarationVisibility.Internal
            DescriptorVisibilities.PROTECTED -> DeclarationVisibility.Protected
            else -> DeclarationVisibility.Private
        }

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

private fun DescriptorVisibility.coerceAtLeast(minimumValue: DescriptorVisibility): DescriptorVisibility? =
    this.compareTo(minimumValue)?.let { comparisonResult -> if (comparisonResult < 0) minimumValue else this }

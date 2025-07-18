package org.jetbrains.kotlin.compiler.plugin.template.ir

import org.jetbrains.kotlin.ir.declarations.IrSimpleFunction
import org.jetbrains.kotlin.ir.types.IrDynamicType
import org.jetbrains.kotlin.ir.types.IrErrorType
import org.jetbrains.kotlin.ir.types.IrSimpleType
import org.jetbrains.kotlin.ir.types.IrType
import org.jetbrains.kotlin.ir.types.classFqName
import org.jetbrains.kotlin.ir.types.typeOrNull
import org.jetbrains.kotlin.ir.util.fqNameWhenAvailable
import org.jetbrains.kotlin.js.parser.sourcemaps.JsonArray
import org.jetbrains.kotlin.js.parser.sourcemaps.JsonObject
import org.jetbrains.kotlin.js.parser.sourcemaps.JsonString
import org.jetbrains.kotlin.name.FqName

internal fun IrSimpleFunction.signatureAsInjectionFunction(): InjectionFunctionSignature = InjectionFunctionSignature(
    name = fqNameWhenAvailable ?: TODO(),
    parameters = valueParameters.map { it.type.toTypeExpression() }
)

internal data class InjectionFunctionSignature(
    val name: FqName,
    val parameters: List<TypeExpression>,
) {
    data class TypeExpression(
        val name: FqName,
        val arguments: List<TypeExpression>,
    )
}

internal fun IrType.toTypeExpression(): InjectionFunctionSignature.TypeExpression = when (this) {
    is IrDynamicType -> TODO()
    is IrErrorType -> TODO()
    is IrSimpleType -> InjectionFunctionSignature.TypeExpression(
        name = this.classFqName ?: error("IrType is not a class"), // TODO: Don't crash on other types
        arguments = arguments.map { it.typeOrNull?.toTypeExpression() ?: error("IrType argument is not a class") },
    )
}

internal fun InjectionFunctionSignature.parseToJsonObject(): JsonObject = JsonObject(
    "name" to JsonString(name.asString()),
    "parameters" to JsonArray(
        parameters.map { it.parseToJsonObject() }.toMutableList()
    )
)

private fun InjectionFunctionSignature.TypeExpression.parseToJsonObject(): JsonObject = JsonObject(
    "name" to JsonString(name.asString()),
    "arguments" to JsonArray(
        arguments.map { it.parseToJsonObject() }.toMutableList()
    )
)
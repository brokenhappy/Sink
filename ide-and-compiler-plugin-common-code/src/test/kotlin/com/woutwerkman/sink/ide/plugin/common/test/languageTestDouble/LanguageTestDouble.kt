package com.woutwerkman.sink.ide.plugin.common.test.languageTestDouble

import com.woutwerkman.sink.ide.plugin.common.TypeVariance
import kotlin.contracts.ExperimentalContracts
import kotlin.contracts.contract

object LanguageTestDouble {
    data class Module(
        val name: String,
        val functions: List<FunctionSymbol>,
        val types: List<TypeSymbol>,
        val dependencies: List<Module>,
    )

    sealed class TypeExpression {
        data class Concrete(
            val symbol: TypeSymbol,
            val arguments: List<TypeExpression>,
        ) : TypeExpression() {
            override fun toString(): String =
                "${symbol.fqName}${if (arguments.isNotEmpty()) "<${arguments.joinToString(", ")}>" else ""}"
        }
        data class Generic(val symbol: TypeParameterSymbol) : TypeExpression() {
            override fun toString(): String = symbol.name
        }
        data object StarProjection : TypeExpression() {
            override fun toString(): String = "*"
        }
        data object BottomType: TypeExpression() {
            override fun toString(): String = "Nothing"
        }
        data object TopType: TypeExpression() {
            override fun toString(): String = "Any?"
        }
        data class InType(val expression: TypeExpression) : TypeExpression() {
            override fun toString(): String = "in $expression"
        }
        data class OutType(val expression: TypeExpression) : TypeExpression() {
            override fun toString(): String = "out $expression"
        }
        data class Nullable(val expression: TypeExpression) : TypeExpression() {
            override fun toString(): String = "$expression?"
        }
    }

    class TypeSymbol(
        val visibility: Visibility,
        val fqName: String,
        val parameters: List<TypeParameterSymbol>,
        val supertypes: List<TypeExpression>,
        var moduleOrNullIfNotBound: Module?,
    ) {
        fun copy(
            visibility: Visibility = this.visibility,
            fqName: String = this.fqName,
            parameters: List<TypeParameterSymbol> = this.parameters,
            supertypes: List<TypeExpression> = this.supertypes,
        ): TypeSymbol = TypeSymbol(visibility, fqName, parameters, supertypes, moduleOrNullIfNotBound)
    }
    val TypeSymbol.module: Module get() = moduleOrNullIfNotBound!!

    class FunctionSymbol(
        val visibility: Visibility,
        val fqName: String,
        val parameters: List<Parameter>,
        val typeParameters: List<TypeParameterSymbol>,
        val returnType: TypeExpression?,
        var moduleOrNullIfNotBound: Module?,
    ) {
        data class Parameter(val name: String, val type: TypeExpression)
        fun copy(
            visibility: Visibility = this.visibility,
            fqName: String = this.fqName,
            parameters: List<Parameter> = this.parameters,
            typeParameters: List<TypeParameterSymbol> = this.typeParameters,
            returnType: TypeExpression? = this.returnType,
        ): FunctionSymbol = FunctionSymbol(visibility, fqName, parameters, typeParameters, returnType, moduleOrNullIfNotBound)
    }
    val FunctionSymbol.module: Module get() = moduleOrNullIfNotBound!!

    class TypeParameterSymbol(val name: String, val variance: TypeVariance, var ownerOrNullIfNotBound: GenericSymbolOwner?)
    val TypeParameterSymbol.owner: GenericSymbolOwner get() = ownerOrNullIfNotBound!!

    sealed class GenericSymbolOwner {
        data class Function(val symbol: FunctionSymbol) : GenericSymbolOwner()
        data class Type(val symbol: TypeSymbol) : GenericSymbolOwner()
    }

    interface ModuleBuilder {
        fun registerType(symbol: TypeSymbol)
        fun deregisterType(symbol: TypeSymbol)
        fun registerFunction(symbol: FunctionSymbol)
        fun deregisterFunction(symbol: FunctionSymbol)
        val Star: TypeExpression.StarProjection get() = TypeExpression.StarProjection
        val Nothing: TypeExpression.BottomType get() = TypeExpression.BottomType
        val AnyQ: TypeExpression.TopType get() = TypeExpression.TopType
    }

    @OptIn(ExperimentalContracts::class)
    fun module(
        name: String = "test",
        dependsOn: List<Module> = emptyList(),
        builder: context(
            TypeExpressionConverter<TypeExpression>,
            TypeExpressionConverter<TypeParameterSymbol>,
            TypeExpressionConverter<TypeSymbol>,
        ) ModuleBuilder.() -> Unit,
    ): Module {
        contract { callsInPlace(builder, kotlin.contracts.InvocationKind.EXACTLY_ONCE) }
        val types = mutableSetOf<TypeSymbol>()
        val functions = mutableSetOf<FunctionSymbol>()
        builder(
            selfTypeExpressionConverter,
            typeParameterSymbolToTypeExpressionConverter,
            typeToTypeExpressionConverter,
            object : ModuleBuilder {
                override fun registerType(symbol: TypeSymbol) {
                    types += symbol
                }

                override fun deregisterType(symbol: TypeSymbol) {
                    types -= symbol
                }

                override fun registerFunction(symbol: FunctionSymbol) {
                    functions += symbol
                }

                override fun deregisterFunction(symbol: FunctionSymbol) {
                    functions -= symbol
                }
            },
        )
        return Module(
            name,
            functions.toList(),
            types.toList(),
            dependsOn,
        ).also { module ->
            fun List<TypeParameterSymbol>.bind(owner: GenericSymbolOwner) {
                forEach { genericSymbol ->
                    assert(genericSymbol.ownerOrNullIfNotBound == null) {
                        "$genericSymbol is already bound to ${genericSymbol.ownerOrNullIfNotBound}"
                    }
                    genericSymbol.ownerOrNullIfNotBound = owner
                }
            }
            module.types.forEach { type ->
                assert(type.moduleOrNullIfNotBound == null) {
                    "$type is already bound to ${type.moduleOrNullIfNotBound}"
                }
                type.parameters.bind(GenericSymbolOwner.Type(type))
                type.moduleOrNullIfNotBound = module
            }
            module.functions.forEach { function ->
                assert(function.moduleOrNullIfNotBound == null) {
                    "$function is already bound to ${function.moduleOrNullIfNotBound}"
                }
                function.typeParameters.bind(GenericSymbolOwner.Function(function))
                function.moduleOrNullIfNotBound = module
            }
        }
    }

    fun interface TypeExpressionAssertionScheduler {
        fun scheduleAssertion(assertion: () -> Unit)
    }

    context(scheduler: TypeExpressionAssertionScheduler)
    fun scheduleAssertion(assertion: () -> Unit) = scheduler.scheduleAssertion(assertion)

    /** Schedules assertions for once the types are fully built. */
    fun withBuiltTypeAssertions(
        builder: context(
            TypeExpressionConverter<TypeExpression>,
            TypeExpressionConverter<TypeParameterSymbol>,
            TypeExpressionConverter<TypeSymbol>,
            TypeExpressionAssertionScheduler,
        ) ModuleBuilder.() -> Unit,
    ) {
        val assertions = mutableListOf<() -> Unit>()
        val scheduler = TypeExpressionAssertionScheduler { assertions += it }
        module {
            with(scheduler) {
                builder()
            }
        }
        assertions.forEach { it() }
    }
}

enum class Visibility { Public, Internal, Private }

val LanguageTestDouble.ModuleBuilder.public: Visibility get() = Visibility.Public
val LanguageTestDouble.ModuleBuilder.internal: Visibility get() = Visibility.Internal
val LanguageTestDouble.ModuleBuilder.private: Visibility get() = Visibility.Private

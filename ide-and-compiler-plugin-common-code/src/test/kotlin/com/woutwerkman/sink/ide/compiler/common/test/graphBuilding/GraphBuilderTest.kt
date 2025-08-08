package com.woutwerkman.sink.ide.compiler.common.test.graphBuilding

import com.woutwerkman.sink.ide.compiler.common.ConcreteType
import com.woutwerkman.sink.ide.compiler.common.DeclarationVisibility
import com.woutwerkman.sink.ide.compiler.common.DependencyGraphBuilder
import com.woutwerkman.sink.ide.compiler.common.FunctionBehavior
import com.woutwerkman.sink.ide.compiler.common.TypeBehavior
import com.woutwerkman.sink.ide.compiler.common.TypeVariance
import com.woutwerkman.sink.ide.compiler.common.WithVariance
import kotlin.reflect.KClass
import kotlin.reflect.KType
import kotlin.reflect.KTypeParameter
import kotlin.reflect.full.allSupertypes
import kotlin.reflect.full.isSubtypeOf
import kotlin.reflect.typeOf
import kotlin.test.Test
import kotlin.test.assertEquals
import kotlin.test.fail

class GraphBuilderTest {
    @Test
    fun `single injectable without dependencies`() {
        class Foo

        buildDiGraph {
            public func "foo".returns<Foo>()
        }.also { graph ->
            graph.instantiatorFunctionsToDependencies.entries.single().also { (func, dependencies) ->
                assert(func.name == "foo")
                assert(dependencies.isEmpty())
            }
        }
    }

    @Test
    fun `two unrelated injectable`() {
        class Foo
        class Bar

        buildDiGraph {
            public func "foo".returns<Foo>()
            public func "bar".returns<Bar>()
        }.also { graph ->
            assert(graph.instantiatorFunctionsToDependencies.size == 2)
            assert(graph.instantiatorFunctionsToDependencies.values.flatten().isEmpty())
        }
    }

    @Test
    fun `simple single injection example`() {
        class Foo
        class Bar

        buildDiGraph {
            public func "foo".returns<Foo>()
            public func "bar"("foo"<Foo>()).returns<Bar>()
        }.also { graph ->
            assert(graph.instantiatorFunctionsToDependencies.size == 2)
            assertEquals(
                "foo",
                graph
                    .instantiatorFunctionsToDependencies
                    .entries
                    .first { it.key.name == "bar" }
                    .value
                    .single()
                    .assertIs<ImplementationDetail>()
                    .instantiatorOrInjectorFunction
                    .name
            )
        }
    }

    @Test
    fun `duplicate type declaration`() {
        class Foo

        buildDiGraph {
            public func "foo".returns<Foo>()
            public func "bar".returns<Foo>()
        }.also { graph ->
            assert(graph.instantiatorFunctionsToDependencies.size == 2)
            assertEquals(
                setOf("bar", "foo"),
                graph.duplicates.single().map { it.name }.toSet(),
            )
        }
    }

    @Test
    fun `cyclic dependency`() {
        class Foo
        class Bar

        buildDiGraph {
            public func "foo"("bar"<Bar>()).returns<Foo>()
            public func "bar"("foo"<Foo>()).returns<Bar>()
        }.also { graph ->
            assert(graph.instantiatorFunctionsToDependencies.size == 2)
            assertEquals(
                setOf("bar", "foo"),
                graph.cycles.single().map { it.name }.toSet(),
            )
        }
    }

    @Test
    fun `dependency by simple subtype`() {
        open class Foo
        class Bar : Foo()
        class Baz

        buildDiGraph {
            public func "bar".returns<Bar>()
            public func "baz"("foo"<Foo>()).returns<Baz>()
        }.also { graph ->
            assert(graph.instantiatorFunctionsToDependencies.size == 2)
            graph
                .dependenciesForFunctionCalled("baz")
                .single()
                .assertIs<ImplementationDetail>()
                .instantiatorOrInjectorFunction
                .name
                .assertIs("bar")
        }
    }
}

private inline fun <reified T> Any?.assertIs(): T =
    if (this is T) this
    else fail("Expected $this to be of type ${T::class.java.name}")

private fun <T> T.assertIs(expected: T) {
    assertEquals(expected, this)
}

private inline fun <T : Any> T?.assertNotNull(message: () -> String): T = this ?: fail(message())

private fun DependencyGraphFromSources.dependenciesForFunctionCalled(
    name: String
): List<DependencyGraphBuilder.ResolvedDependency<KType, FunctionSymbol>> =
    instantiatorFunctionsToDependencies
        .entries
        .firstOrNull { it.key.name == name }
        .assertNotNull { "No function with name $name found among ${instantiatorFunctionsToDependencies.keys.map { it.name }}" }
        .value

interface TestDoubleFunctionBuilder {
    val public: DeclarationVisibility get() = DeclarationVisibility.Public
    val internal: DeclarationVisibility get() = DeclarationVisibility.Internal
    val private: DeclarationVisibility get() = DeclarationVisibility.Private

    infix fun DeclarationVisibility.func(signature: FunctionSignatureAndReturnType)
}

context(_: TestDoubleFunctionBuilder)
operator fun String.invoke(vararg params: Parameter): FunctionSignature = FunctionSignature(this, params.toList())

context(_: TestDoubleFunctionBuilder)
inline operator fun <reified T> String.invoke(): Parameter = Parameter(this, typeOf<T>())

context(_: TestDoubleFunctionBuilder)
inline fun <reified T> FunctionSignature.returns(): FunctionSignatureAndReturnType =
    FunctionSignatureAndReturnType(this, typeOf<T>())

context(_: TestDoubleFunctionBuilder)
inline fun <reified T> String.returns(): FunctionSignatureAndReturnType =
    FunctionSignatureAndReturnType(FunctionSignature(this, parameters = emptyList()), typeOf<T>())

data class FunctionSymbol(
    val visibility: DeclarationVisibility,
    val name: String,
    val parameters: List<Parameter>,
    val returnType: KType,
    var moduleOrNullIfUnbound: Any? = null,
)

data class Parameter(val name: String, val type: KType)
data class FunctionSignature(val name: String, val parameters: List<Parameter>)
data class FunctionSignatureAndReturnType(val signature: FunctionSignature, val returnType: KType)

fun buildDiGraph(
    builder: TestDoubleFunctionBuilder.() -> Unit,
): DependencyGraphFromSources =
    DependencyGraphBuilder(TestDoubleFunctionAsInjectableBehavior, KTypeBehavior).buildGraph(
        injectablesOfThisModule = buildList {
            val module = object {}
            builder(object: TestDoubleFunctionBuilder {
                override fun DeclarationVisibility.func(signature: FunctionSignatureAndReturnType) {
                    add(FunctionSymbol(
                        visibility = this,
                        signature.signature.name,
                        signature.signature.parameters,
                        signature.returnType,
                    ).also { it.moduleOrNullIfUnbound = module })
                }
            })
        },
        moduleDependencyGraphs = emptyList(),
    )

private typealias DependencyGraphFromSources = com.woutwerkman.sink.ide.compiler.common.DependencyGraphFromSources<FunctionSymbol, KType, KClass<*>>
private typealias ImplementationDetail = com.woutwerkman.sink.ide.compiler.common.DependencyGraphBuilder.ResolvedDependency.ImplementationDetail<KType, FunctionSymbol>


object TestDoubleFunctionAsInjectableBehavior : FunctionBehavior<KType, FunctionSymbol> {
    override fun getReturnTypeOf(injectable: FunctionSymbol): KType = injectable.returnType
    override fun getFqnOf(injectable: FunctionSymbol): String = injectable.name
    override fun getModuleOf(injectable: FunctionSymbol): Any? = injectable.moduleOrNullIfUnbound

    override fun getParametersOf(injectable: FunctionSymbol): List<Pair<String, KType>> = injectable
        .parameters
        .map { it.name to it.type }
}

object KTypeBehavior: TypeBehavior<KType, KClass<*>, KTypeParameter> {
    override fun getFqnOf(symbol: KClass<*>): String = symbol.qualifiedName ?: symbol.simpleName!!
    override fun asConcreteType(type: KType): ConcreteType<KClass<*>, KType>? =
        (type.classifier as? KClass<*>)?.let { ConcreteType(it, emptyList()) }

    override fun isSubtype(subtype: KType, supertype: KType): Boolean = subtype.isSubtypeOf(supertype)
    override fun getMinimumVisibilityOf(expression: KType): DeclarationVisibility = TODO()
    override fun getVarianceOf(typeParameter: KTypeParameter): TypeVariance = TODO("Not yet implemented")
    override fun superTypesOfWithoutAny(symbol: KClass<*>): Sequence<KType> = symbol.allSupertypes.asSequence()
    override fun getTypeParameterSymbolsOf(symbol: KClass<*>): List<KTypeParameter> = TODO("Not yet implemented")
    override fun isStarProjection(type: KType): Boolean = TODO("Not yet implemented")
    override fun unwrapNullableOrNull(type: KType): KType? = TODO("Not yet implemented")
    override fun isTopType(type: KType): Boolean = TODO("Not yet implemented")
    override fun isBottomType(type: KType): Boolean = TODO("Not yet implemented")
    override fun asTypeParameterReference(type: KType): KTypeParameter? = TODO("Not yet implemented")
    override fun unwrapVarianceOrNull(type: KType): WithVariance<KType>? = TODO("Not yet implemented")
}
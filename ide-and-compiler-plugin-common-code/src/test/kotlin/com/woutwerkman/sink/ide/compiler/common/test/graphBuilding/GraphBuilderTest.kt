package com.woutwerkman.sink.ide.compiler.common.test.graphBuilding

import com.woutwerkman.sink.ide.compiler.common.ConcreteType
import com.woutwerkman.sink.ide.compiler.common.DeclarationVisibility
import com.woutwerkman.sink.ide.compiler.common.DependencyGraphBuilder
import com.woutwerkman.sink.ide.compiler.common.FunctionBehavior
import com.woutwerkman.sink.ide.compiler.common.TypeBehavior
import com.woutwerkman.sink.ide.compiler.common.TypeVariance
import com.woutwerkman.sink.ide.compiler.common.WithVariance
import com.woutwerkman.sink.ide.compiler.common.injectorFunctionNameOf
import com.woutwerkman.sink.ide.compiler.common.moduleDependencyGraphFromBytes
import com.woutwerkman.sink.ide.compiler.common.parameterName
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
    fun `empty module yields empty graph`() {
        buildDiGraph {
            // no functions
        }.also { graph ->
            assert(graph.instantiatorFunctionsToDependencies.isEmpty())
            assert(graph.cycles.isEmpty())
            assert(graph.duplicates.isEmpty())
        }
    }

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
            graph.instantiatorFunctionsToDependencies.values.also { dependencies ->
                dependencies.size.assertIs(2)
                dependencies.flatten().assert { it.isEmpty() }
            }
        }
    }

    @Test
    fun `unresolved dependency becomes external`() {
        class Foo
        class Bar

        buildDiGraph {
            public func "bar"("foo"<Foo>()).returns<Bar>()
        }.also { graph ->
            graph
                .dependenciesForFunctionCalled("bar")
                .single()
                .assertIs<ExternalDependency>()
                .also { externalDependency ->
                    externalDependency.parameterName.assertIs("foo")
                    externalDependency.type.assertIs(typeOf<Foo>())
                }
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
            graph
                .dependenciesForFunctionCalled("bar")
                .single()
                .assertIs<ImplementationDetail>()
                .instantiatorOrInjectorFunction
                .name
                .assertIs("foo")
        }
    }

    @Test
    fun `duplicate type declaration`() {
        class Foo

        buildDiGraph {
            public func "foo".returns<Foo>()
            public func "bar".returns<Foo>()
        }.also { graph ->
            graph.instantiatorFunctionsToDependencies.size.assertIs(2)
            graph
                .duplicates
                .single()
                .map { it.name }
                .toSet()
                .assertIs(setOf("bar", "foo"))
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
            graph.instantiatorFunctionsToDependencies.size.assertIs(2)
            graph
                .cycles
                .single()
                .map { it.name }
                .toSet()
                .assertIs(setOf("bar", "foo"))
        }
    }

    @Test
    fun `double transitive dependency does not duplicate dependency`() {
        class Foo
        class Bar
        class Baz
        class Foobs

        buildDiGraph {
            public func "foo"("foobs"<Foobs>()).returns<Foo>()
            public func "bar"("foobs"<Foobs>()).returns<Bar>()
            public func "baz"("bar"<Bar>(), "foo"<Foo>()).returns<Baz>()
        }.also { graph ->
            graph.instantiatorFunctionsToDependencies.size.assertIs(3)
            graph
                .dependenciesForFunctionCalled("baz")
                .assert { it.size == 3 }
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
            graph.instantiatorFunctionsToDependencies.size.assertIs(2)
            graph
                .dependenciesForFunctionCalled("baz")
                .single()
                .assertIs<ImplementationDetail>()
                .instantiatorOrInjectorFunction
                .name
                .assertIs("bar")
        }
    }

    @Test
    fun `parameter name implicitly duplicated through transitive external dependency creates unique name`() {
        class Foo
        class Bar
        class Baz

        buildDiGraph {
            public func "bar"("foo"<Foo>()).returns<Bar>()
            public func "baz"("foo"<Bar>()).returns<Baz>()
        }.also { graph ->
            graph
                .dependenciesForFunctionCalled("baz")
                .map { it.parameterName }
                .toSet()
                .assert { "foo" in it } // Probably == setOf("foo", "foo0"), but we don't care about the rename ...
                .size
                .assertIs(2) // ... as long as it's unique
        }
    }

    @Test
    fun `multiple parameters resolved in order`() {
        class Foo
        class Bar
        class Baz

        buildDiGraph {
            public func "foo".returns<Foo>()
            public func "bar".returns<Bar>()
            public func "baz"("foo"<Foo>(), "bar"<Bar>()).returns<Baz>()
        }.also { graph ->
            val deps = graph.dependenciesForFunctionCalled("baz")
            deps.size.assertIs(2)
            deps[0].assertIs<ImplementationDetail>().instantiatorOrInjectorFunction.name.assertIs("foo")
            deps[1].assertIs<ImplementationDetail>().instantiatorOrInjectorFunction.name.assertIs("bar")
        }
    }

    @Test
    fun `transitive external dependency chain is captured`() {
        class Foo
        class Bar
        class Baz

        buildDiGraph {
            public func "bar"("foo"<Foo>()).returns<Bar>()
            public func "baz"("bar"<Bar>()).returns<Baz>()
        }.also { graph ->
            val bazDependencies = graph.dependenciesForFunctionCalled("baz").assert { it.size == 2 }
            val fooDep = bazDependencies.filterIsInstance<ExternalDependency>().single()
            val barDep = bazDependencies.filterIsInstance<ImplementationDetail>().single()
            fooDep.parameterName.assertIs("foo")
            fooDep.type.assertIs(typeOf<Foo>())
            barDep.parameterName.assertIs("bar")
        }
    }

    @Test
    fun `dependency by interface subtype`() {
        abstract class Foo
        class Bar: Foo()
        class Baz

        buildDiGraph {
            public func "bar".returns<Bar>()
            public func "baz"("foo"<Foo>()).returns<Baz>()
        }.also { graph ->
            val dep = graph.dependenciesForFunctionCalled("baz").single().assertIs<ImplementationDetail>()
            dep.instantiatorOrInjectorFunction.name.assertIs("bar")
        }
    }

    @Test
    fun `cross module dependency is resolved by this module`() {
        class Foo
        class Bar
        class Baz

        buildDiGraph(
            buildDiGraph {
                public func "bar"("foo"<Foo>()).returns<Bar>()
            }.toModuleGraph(),
        ) {
            public func "foo".returns<Foo>()
            public func "baz"("bar"<Bar>()).returns<Baz>()
        }.also { graph ->
            val bazDep = graph.dependenciesForFunctionCalled("baz").single().assertIs<ImplementationDetail>()
            bazDep.instantiatorOrInjectorFunction.name.assertIs("Bar")
        }
    }
}

private inline fun <reified T> Any?.assertIs(): T =
    if (this is T) this
    else fail("Expected $this to be of type ${T::class.java.name}")

private fun <T> T.assertIs(expected: T) {
    assertEquals(expected, this)
}

private fun <T> T.assert(assertion: (T) -> Boolean): T =
    if (!assertion(this)) fail("Assertion failed for $this")
    else this

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
    vararg dependencies: ModuleDependencyGraph,
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
        moduleDependencyGraphs = dependencies.toList(),
    )

private fun DependencyGraphFromSources.toModuleGraph(): ModuleDependencyGraph {
    val resolverMap = instantiatorFunctionsToDependencies.keys.associate { original ->
        val fqn = KTypeBehavior.injectorFunctionNameOf(original.returnType)
        fqn to FunctionSymbol(
            visibility = DeclarationVisibility.Public,
            name = fqn,
            parameters = original.parameters,
            returnType = original.returnType,
        )
    }
    return context(TestDoubleFunctionAsInjectableBehavior, KTypeBehavior) {
        moduleDependencyGraphFromBytes(
            injectorResolver = { name: String -> resolverMap[name] ?: error("Unknown injector $name") },
            bytes = serializeAsModuleDependencyGraph(),
        )
    }
}

private typealias DependencyGraphFromSources = com.woutwerkman.sink.ide.compiler.common.DependencyGraphFromSources<FunctionSymbol, KType, KClass<*>>
private typealias ModuleDependencyGraph = com.woutwerkman.sink.ide.compiler.common.ModuleDependencyGraph<FunctionSymbol, KType, KClass<*>>
private typealias ImplementationDetail = com.woutwerkman.sink.ide.compiler.common.DependencyGraphBuilder.ResolvedDependency.ImplementationDetail<KType, FunctionSymbol>
private typealias ExternalDependency = com.woutwerkman.sink.ide.compiler.common.DependencyGraphBuilder.ResolvedDependency.ExternalDependency<KType, FunctionSymbol>


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
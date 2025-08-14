package com.woutwerkman.sink.ide.compiler.common.test.graphBuilding

import com.woutwerkman.sink.ide.compiler.common.ConcreteType
import com.woutwerkman.sink.ide.compiler.common.DeclarationContainerBehavior
import com.woutwerkman.sink.ide.compiler.common.DeclarationVisibility
import com.woutwerkman.sink.ide.compiler.common.DependencyGraphBuilder
import com.woutwerkman.sink.ide.compiler.common.FunctionBehavior
import com.woutwerkman.sink.ide.compiler.common.ModulesDependencyGraph
import com.woutwerkman.sink.ide.compiler.common.TypeBehavior
import com.woutwerkman.sink.ide.compiler.common.TypeVariance
import com.woutwerkman.sink.ide.compiler.common.WithVariance
import com.woutwerkman.sink.ide.compiler.common.injectorFunctionNameOf
import com.woutwerkman.sink.ide.compiler.common.addFromBytes
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
    fun `can resolve public injectable through object`() {
        class Foo
        class Bar

        buildDiGraph {
            public func "foo"("bar"<Bar>()).returns<Foo>()

            public objec {
                public func "bar".returns<Bar>()
            }
        }.also { graph ->
            graph
                .dependenciesForFunctionCalled("foo")
                .single()
                .assertIs<ImplementationDetail>()
                .assert { it.instantiatorOrInjectorFunction.name == "bar" }
        }
    }

    @Test
    fun `can resolve public injectable through single level private object`() {
        class Foo
        class Bar

        buildDiGraph {
            public func "foo"("bar"<Bar>()).returns<Foo>()

            private objec {
                public func "bar".returns<Bar>()
            }
        }.also { graph ->
            graph
                .dependenciesForFunctionCalled("foo")
                .single()
                .assertIs<ImplementationDetail>()
                .assert { it.instantiatorOrInjectorFunction.name == "bar" }
        }
    }

    @Test
    fun `can not resolve public injectable through double level private object`() {
        class Foo
        class Bar

        buildDiGraph {
            public func "foo"("bar"<Bar>()).returns<Foo>()

            public objec {
                private objec {
                    public func "bar".returns<Bar>()
                }
            }
        }.also { graph ->
            graph
                .dependenciesForFunctionCalled("foo")
                .single()
                .assertIs<ExternalDependency>()
        }
    }

    @Test
    fun `can not resolve private injectable through public object`() {
        class Foo
        class Bar

        buildDiGraph {
            public func "foo"("bar"<Bar>()).returns<Foo>()

            public objec {
                private func "bar".returns<Bar>()
            }
        }.also { graph ->
            graph
                .dependenciesForFunctionCalled("foo")
                .single()
                .assertIs<ExternalDependency>()
        }
    }

    @Test
    fun `resolve to the outside, even inner is private`() {
        class Foo
        class Bar

        buildDiGraph {
            public func "bar".returns<Bar>()

            public objec {
                private func "foo"("bar"<Bar>()).returns<Foo>()
            }
        }.also { graph ->
            graph
                .dependenciesForFunctionCalled("foo")
                .single()
                .assertIs<ImplementationDetail>()
                .assert { it.instantiatorOrInjectorFunction.name == "bar" }
        }
    }

    @Test
    fun `resolve to the outside, even if everything is private`() {
        class Foo
        class Bar

        buildDiGraph {
            private func "bar".returns<Bar>()

            private objec {
                private func "foo"("bar"<Bar>()).returns<Foo>()
            }
        }.also { graph ->
            graph
                .dependenciesForFunctionCalled("foo")
                .single()
                .assertIs<ImplementationDetail>()
                .assert { it.instantiatorOrInjectorFunction.name == "bar" }
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
            val barDep = graph.dependenciesForFunctionCalled("baz").single().assertIs<ImplementationDetail>()
            val fooDep = barDep.transitiveDependencies.single().assertIs<ExternalDependency>()
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
): List<DependencyGraphBuilder.ResolvedDependency<KType, FunctionSymbol, KClass<*>, DeclarationContainer>> =
    dependenciesForFunctionCalledOrNull(name)
        .let { it ?: children.firstNotNullOfOrNull { it.dependenciesForFunctionCalledOrNull(name) } }
        .assertNotNull { "No function with name $name found among ${instantiatorFunctionsToDependencies.keys.map { it.name }}" }
        .value

private fun DependencyGraphFromSources.dependenciesForFunctionCalledOrNull(
    name: String
): Map.Entry<FunctionSymbol, List<DependencyGraphBuilder.ResolvedDependency<KType, FunctionSymbol, KClass<*>, DeclarationContainer>>>? =
    instantiatorFunctionsToDependencies
        .entries
        .firstOrNull { it.key.name == name }

interface TestDoubleFunctionBuilder {
    val public: DeclarationVisibility get() = DeclarationVisibility.Public
    val internal: DeclarationVisibility get() = DeclarationVisibility.Internal
    val private: DeclarationVisibility get() = DeclarationVisibility.Private

    infix fun DeclarationVisibility.func(signature: FunctionSignatureAndReturnType)
    infix fun DeclarationVisibility.objec(builder: TestDoubleFunctionBuilder.() -> Unit)
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
)

data class Parameter(val name: String, val type: KType)
data class FunctionSignature(val name: String, val parameters: List<Parameter>)
data class FunctionSignatureAndReturnType(val signature: FunctionSignature, val returnType: KType)

private fun buildDiGraph(
    modulesDependencyGraph: ModuleDependencyGraph = ModuleDependencyGraph(),
    builder: TestDoubleFunctionBuilder.() -> Unit,
): DependencyGraphFromSources =
    context(TestDoubleFunctionAsInjectableBehavior, KTypeBehavior, DeclarationContainerBehavior) {
        DependencyGraphBuilder<KType, FunctionSymbol, KClass<*>, DeclarationContainer>().buildGraph(
            buildInjectableContainer(DeclarationVisibility.Public, builder),
            modulesDependencyGraph = modulesDependencyGraph,
        )
    }

private fun buildInjectableContainer(visibility: DeclarationVisibility, builder: TestDoubleFunctionBuilder.() -> Unit): DeclarationContainer =
    context(TestDoubleFunctionAsInjectableBehavior, KTypeBehavior, DeclarationContainerBehavior) {
        val injectables = mutableListOf<FunctionSymbol>()
        val childContainers = mutableListOf<DeclarationContainer>()
        builder(object : TestDoubleFunctionBuilder {
            override fun DeclarationVisibility.func(signature: FunctionSignatureAndReturnType) {
                injectables.add(
                    FunctionSymbol(
                        visibility = this,
                        signature.signature.name,
                        signature.signature.parameters,
                        signature.returnType,
                    )
                )
            }

            override fun DeclarationVisibility.objec(builder: TestDoubleFunctionBuilder.() -> Unit) {
                childContainers.add(buildInjectableContainer(this, builder))
            }
        })
        DeclarationContainer(visibility, injectables, childContainers)
    }

private fun DependencyGraphFromSources.toModuleGraph(): ModuleDependencyGraph {
    val resolverMap = buildMap {
        forEachInjectable { original ->
            val fqn = KTypeBehavior.injectorFunctionNameOf(original.returnType)
            this[fqn] = FunctionSymbol(
                visibility = DeclarationVisibility.Public,
                name = fqn,
                parameters = original.parameters,
                returnType = original.returnType,
            )
        }
    }
    return context(TestDoubleFunctionAsInjectableBehavior, KTypeBehavior) {
        ModuleDependencyGraph().apply {
            addFromBytes(
                injectorResolver = { name: String -> resolverMap[name] ?: error("Unknown injector $name") },
                bytes = serializeAsModuleDependencyGraph(),
            )
        }
    }
}

private data class DeclarationContainer(
    val visibility: DeclarationVisibility,
    val injectables: List<FunctionSymbol>,
    val childContainers: MutableList<DeclarationContainer>,
)

private typealias DependencyGraphFromSources = com.woutwerkman.sink.ide.compiler.common.DependencyGraphFromSources<FunctionSymbol, KType, KClass<*>, DeclarationContainer>
private typealias ModuleDependencyGraph = ModulesDependencyGraph<FunctionSymbol, KType, KClass<*>, DeclarationContainer>
private typealias ImplementationDetail = DependencyGraphBuilder.ResolvedDependency.ImplementationDetail<KType, FunctionSymbol, KClass<*>, DeclarationContainer>
private typealias ExternalDependency = DependencyGraphBuilder.ResolvedDependency.ExternalDependency<KType, FunctionSymbol, KClass<*>, DeclarationContainer>


object TestDoubleFunctionAsInjectableBehavior : FunctionBehavior<KType, FunctionSymbol> {
    override fun getReturnTypeOf(injectable: FunctionSymbol): KType = injectable.returnType
    override fun getFqnOf(injectable: FunctionSymbol): String = injectable.name
    override fun getVisibilityOf(injectable: FunctionSymbol): DeclarationVisibility = injectable.visibility
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

private object DeclarationContainerBehavior: DeclarationContainerBehavior<DeclarationContainer, FunctionSymbol> {
    override fun getChildrenOf(container: DeclarationContainer): List<DeclarationContainer> = container.childContainers
    override fun getDeclarationsOf(container: DeclarationContainer): List<FunctionSymbol> = container.injectables
    override fun getVisibilityOf(container: DeclarationContainer): DeclarationVisibility = container.visibility
}
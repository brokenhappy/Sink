package com.woutwerkman.sink.ide.compiler.common.test.languageTestDouble

import com.woutwerkman.sink.ide.compiler.common.ConcreteType
import com.woutwerkman.sink.ide.compiler.common.DeclarationVisibility
import com.woutwerkman.sink.ide.compiler.common.TypeBehavior
import com.woutwerkman.sink.ide.compiler.common.TypeVariance
import com.woutwerkman.sink.ide.compiler.common.WithVariance
import com.woutwerkman.sink.ide.compiler.common.hasTypeRelation
import com.woutwerkman.sink.ide.compiler.common.test.languageTestDouble.LanguageTestDouble.TypeExpression
import com.woutwerkman.sink.ide.compiler.common.test.languageTestDouble.LanguageTestDouble.TypeParameterSymbol
import com.woutwerkman.sink.ide.compiler.common.test.languageTestDouble.LanguageTestDouble.TypeSymbol
import com.woutwerkman.sink.ide.compiler.common.test.languageTestDouble.LanguageTestDouble.withBuiltTypeAssertions
import kotlin.experimental.ExperimentalTypeInference
import kotlin.reflect.KType
import kotlin.reflect.full.isSubtypeOf
import kotlin.reflect.typeOf
import kotlin.test.Test
import kotlin.test.fail

@Suppress("LocalVariableName")
class TestTypeTestDoublesSubtypes {
    private fun TypeExpression.isSubtypeOf(other: TypeExpression): Boolean =
        TestDoubleTypeBehavior.hasTypeRelation(lhs = this, rhs = other, variance = TypeVariance.Covariant)

    context(scheduler: LanguageTestDouble.TypeExpressionAssertionScheduler)
    private inline fun <reified Lhs, reified Rhs> List<TypeExpression>.haveSameTypeRelationAs() {
        assert(this.size == 2) { "Expected exactly two type expressions" }
        scheduler.scheduleAssertion {
            val (Lhs, Rhs) = this
            val LhsExpected = typeOf<Lhs>()
            val RhsExpected = typeOf<Rhs>()
            assertSubtypeRelation(LhsExpected, RhsExpected, Lhs, Rhs)
            assertSubtypeRelation(RhsExpected, LhsExpected, Rhs, Lhs)
        }
    }

    private fun assertSubtypeRelation(
        LhsExpected: KType,
        RhsExpected: KType,
        Lhs: TypeExpression,
        Rhs: TypeExpression,
    ) {
        assertEquals(LhsExpected.isSubtypeOf(RhsExpected), Lhs.isSubtypeOf(Rhs)) {
            """
                Expected $Lhs${if (LhsExpected.isSubtypeOf(RhsExpected)) "" else " NOT"} to be a subtype of $Rhs
                The relation was compared to $LhsExpected and $RhsExpected 
            """.trimIndent()
        }
    }

    @Test
    fun `star projection against In of AnyQ`() {
        withBuiltTypeAssertions {
            class In<in T>

            val In = public type "In"("in T")


            (In(AnyQ) and In(Star)).haveSameTypeRelationAs<In<Any?>, In<*>>()
        }
    }

    @Test
    fun `star projection against In of Foo`() {
        withBuiltTypeAssertions {
            class Foo
            class In<in T>

            val Foo = public type "Foo"
            val In = public type "In"("in T")


            (In(Foo) and In(Star)).haveSameTypeRelationAs<In<Foo>, In<*>>()
        }
    }

    @Test
    fun `star projection against In of Nothing`() {
        withBuiltTypeAssertions {
            class In<in T>

            val In = public type "In"("in T")


            (In(Nothing) and In(Star)).haveSameTypeRelationAs<In<Nothing>, In<*>>()
        }
    }

    @Test
    fun `star projection against Inv of In of AnyQ`() {
        withBuiltTypeAssertions {
            class In<in T>
            class Inv<T>

            val In = public type "In"("in T")
            val Inv = public type "Inv"("T")


            (Inv(In(AnyQ)) and Inv(In(Star))).haveSameTypeRelationAs<Inv<In<Any?>>, Inv<In<*>>>()
        }
    }

    @Test
    fun `star projection against Inv of In of Foo`() {
        withBuiltTypeAssertions {
            class Foo
            class In<in T>
            class Inv<T>

            val Foo = public type "Foo"
            val In = public type "In"("in T")
            val Inv = public type "Inv"("T")


            (Inv(In(Foo)) and Inv(In(Star))).haveSameTypeRelationAs<Inv<In<Foo>>, Inv<In<*>>>()
        }
    }

    @Test
    fun `star projection against Inv of In of Nothing`() {
        withBuiltTypeAssertions {
            class In<in T>
            class Inv<T>

            val In = public type "In"("in T")
            val Inv = public type "Inv"("T")


            (Inv(In(Nothing)) and Inv(In(Star))).haveSameTypeRelationAs<Inv<In<Nothing>>, Inv<In<*>>>()
        }
    }

    @Test
    fun `star projection against In of In of AnyQ`() {
        withBuiltTypeAssertions {
            class In<in T>

            val In = public type "In"("in T")


            (In(In(AnyQ)) and In(In(Star))).haveSameTypeRelationAs<In<In<Any?>>, In<In<*>>>()
        }
    }

    @Test
    fun `star projection against In of In of Foo`() {
        withBuiltTypeAssertions {
            class Foo
            class In<in T>

            val Foo = public type "Foo"
            val In = public type "In"("in T")


            (In(In(Foo)) and In(In(Star))).haveSameTypeRelationAs<In<In<Foo>>, In<In<*>>>()
        }
    }

    @Test
    fun `star projection against In of In of Nothing`() {
        withBuiltTypeAssertions {
            class In<in T>

            val In = public type "In"("in T")


            (In(In(Nothing)) and In(In(Star))).haveSameTypeRelationAs<In<In<Nothing>>, In<In<*>>>()
        }
    }

    @Test
    fun `star projection against Out of In of AnyQ`() {
        withBuiltTypeAssertions {
            class In<in T>
            class Out<out T>

            val In = public type "In"("in T")
            val Out = public type "Out"("out T")


            (Out(In(AnyQ)) and Out(In(Star))).haveSameTypeRelationAs<Out<In<Any?>>, Out<In<*>>>()
        }
    }

    @Test
    fun `star projection against Out of In of Foo`() {
        withBuiltTypeAssertions {
            class Foo
            class In<in T>
            class Out<out T>

            val Foo = public type "Foo"
            val In = public type "In"("in T")
            val Out = public type "Out"("out T")


            (Out(In(Foo)) and Out(In(Star))).haveSameTypeRelationAs<Out<In<Foo>>, Out<In<*>>>()
        }
    }

    @Test
    fun `star projection against Out of In of Nothing`() {
        withBuiltTypeAssertions {
            class In<in T>
            class Out<out T>

            val In = public type "In"("in T")
            val Out = public type "Out"("out T")


            (Out(In(Nothing)) and Out(In(Star))).haveSameTypeRelationAs<Out<In<Nothing>>, Out<In<*>>>()
        }
    }

    @Test
    fun `star projection against Inv of AnyQ`() {
        withBuiltTypeAssertions {
            class Inv<T>

            val Inv = public type "Inv"("T")


            (Inv(AnyQ) and Inv(Star)).haveSameTypeRelationAs<Inv<Any?>, Inv<*>>()
        }
    }

    @Test
    fun `star projection against Inv of Foo`() {
        withBuiltTypeAssertions {
            class Foo
            class Inv<T>

            val Foo = public type "Foo"
            val Inv = public type "Inv"("T")


            (Inv(Foo) and Inv(Star)).haveSameTypeRelationAs<Inv<Foo>, Inv<*>>()
        }
    }

    @Test
    fun `star projection against Inv of Nothing`() {
        withBuiltTypeAssertions {
            class Inv<T>

            val Inv = public type "Inv"("T")


            (Inv(Nothing) and Inv(Star)).haveSameTypeRelationAs<Inv<Nothing>, Inv<*>>()
        }
    }

    @Test
    fun `star projection against Inv of Inv of AnyQ`() {
        withBuiltTypeAssertions {
            class Inv<T>

            val Inv = public type "Inv"("T")


            (Inv(Inv(AnyQ)) and Inv(Inv(Star))).haveSameTypeRelationAs<Inv<Inv<Any?>>, Inv<Inv<*>>>()
        }
    }

    @Test
    fun `star projection against Inv of Inv of Foo`() {
        withBuiltTypeAssertions {
            class Foo
            class Inv<T>

            val Foo = public type "Foo"
            val Inv = public type "Inv"("T")


            (Inv(Inv(Foo)) and Inv(Inv(Star))).haveSameTypeRelationAs<Inv<Inv<Foo>>, Inv<Inv<*>>>()
        }
    }

    @Test
    fun `star projection against Inv of Inv of Nothing`() {
        withBuiltTypeAssertions {
            class Inv<T>

            val Inv = public type "Inv"("T")


            (Inv(Inv(Nothing)) and Inv(Inv(Star))).haveSameTypeRelationAs<Inv<Inv<Nothing>>, Inv<Inv<*>>>()
        }
    }

    @Test
    fun `star projection against In of Inv of AnyQ`() {
        withBuiltTypeAssertions {
            class In<in T>
            class Inv<T>

            val In = public type "In"("in T")
            val Inv = public type "Inv"("T")


            (In(Inv(AnyQ)) and In(Inv(Star))).haveSameTypeRelationAs<In<Inv<Any?>>, In<Inv<*>>>()
        }
    }

    @Test
    fun `star projection against In of Inv of Foo`() {
        withBuiltTypeAssertions {
            class Foo
            class In<in T>
            class Inv<T>

            val Foo = public type "Foo"
            val In = public type "In"("in T")
            val Inv = public type "Inv"("T")


            (In(Inv(Foo)) and In(Inv(Star))).haveSameTypeRelationAs<In<Inv<Foo>>, In<Inv<*>>>()
        }
    }

    @Test
    fun `star projection against In of Inv of Nothing`() {
        withBuiltTypeAssertions {
            class In<in T>
            class Inv<T>

            val In = public type "In"("in T")
            val Inv = public type "Inv"("T")


            (In(Inv(Nothing)) and In(Inv(Star))).haveSameTypeRelationAs<In<Inv<Nothing>>, In<Inv<*>>>()
        }
    }

    @Test
    fun `star projection against Out of Inv of AnyQ`() {
        withBuiltTypeAssertions {
            class Inv<T>
            class Out<out T>

            val Inv = public type "Inv"("T")
            val Out = public type "Out"("out T")


            (Out(Inv(AnyQ)) and Out(Inv(Star))).haveSameTypeRelationAs<Out<Inv<Any?>>, Out<Inv<*>>>()
        }
    }

    @Test
    fun `star projection against Out of Inv of Foo`() {
        withBuiltTypeAssertions {
            class Foo
            class Inv<T>
            class Out<out T>

            val Foo = public type "Foo"
            val Inv = public type "Inv"("T")
            val Out = public type "Out"("out T")


            (Out(Inv(Foo)) and Out(Inv(Star))).haveSameTypeRelationAs<Out<Inv<Foo>>, Out<Inv<*>>>()
        }
    }

    @Test
    fun `star projection against Out of Inv of Nothing`() {
        withBuiltTypeAssertions {
            class Inv<T>
            class Out<out T>

            val Inv = public type "Inv"("T")
            val Out = public type "Out"("out T")


            (Out(Inv(Nothing)) and Out(Inv(Star))).haveSameTypeRelationAs<Out<Inv<Nothing>>, Out<Inv<*>>>()
        }
    }

    @Test
    fun `star projection against Out of AnyQ`() {
        withBuiltTypeAssertions {
            class Out<out T>

            val Out = public type "Out"("out T")


            (Out(AnyQ) and Out(Star)).haveSameTypeRelationAs<Out<Any?>, Out<*>>()
        }
    }

    @Test
    fun `star projection against Out of Foo`() {
        withBuiltTypeAssertions {
            class Foo
            class Out<out T>

            val Foo = public type "Foo"
            val Out = public type "Out"("out T")


            (Out(Foo) and Out(Star)).haveSameTypeRelationAs<Out<Foo>, Out<*>>()
        }
    }

    @Test
    fun `star projection against Out of Nothing`() {
        withBuiltTypeAssertions {
            class Out<out T>

            val Out = public type "Out"("out T")


            (Out(Nothing) and Out(Star)).haveSameTypeRelationAs<Out<Nothing>, Out<*>>()
        }
    }

    @Test
    fun `star projection against Inv of Out of AnyQ`() {
        withBuiltTypeAssertions {
            class Inv<T>
            class Out<out T>

            val Inv = public type "Inv"("T")
            val Out = public type "Out"("out T")


            (Inv(Out(AnyQ)) and Inv(Out(Star))).haveSameTypeRelationAs<Inv<Out<Any?>>, Inv<Out<*>>>()
        }
    }

    @Test
    fun `star projection against Inv of Out of Foo`() {
        withBuiltTypeAssertions {
            class Foo
            class Inv<T>
            class Out<out T>

            val Foo = public type "Foo"
            val Inv = public type "Inv"("T")
            val Out = public type "Out"("out T")


            (Inv(Out(Foo)) and Inv(Out(Star))).haveSameTypeRelationAs<Inv<Out<Foo>>, Inv<Out<*>>>()
        }
    }

    @Test
    fun `star projection against Inv of Out of Nothing`() {
        withBuiltTypeAssertions {
            class Inv<T>
            class Out<out T>

            val Inv = public type "Inv"("T")
            val Out = public type "Out"("out T")


            (Inv(Out(Nothing)) and Inv(Out(Star))).haveSameTypeRelationAs<Inv<Out<Nothing>>, Inv<Out<*>>>()
        }
    }

    @Test
    fun `star projection against In of Out of AnyQ`() {
        withBuiltTypeAssertions {
            class In<in T>
            class Out<out T>

            val In = public type "In"("in T")
            val Out = public type "Out"("out T")


            (In(Out(AnyQ)) and In(Out(Star))).haveSameTypeRelationAs<In<Out<Any?>>, In<Out<*>>>()
        }
    }

    @Test
    fun `star projection against In of Out of Foo`() {
        withBuiltTypeAssertions {
            class Foo
            class In<in T>
            class Out<out T>

            val Foo = public type "Foo"
            val In = public type "In"("in T")
            val Out = public type "Out"("out T")


            (In(Out(Foo)) and In(Out(Star))).haveSameTypeRelationAs<In<Out<Foo>>, In<Out<*>>>()
        }
    }

    @Test
    fun `star projection against In of Out of Nothing`() {
        withBuiltTypeAssertions {
            class In<in T>
            class Out<out T>

            val In = public type "In"("in T")
            val Out = public type "Out"("out T")


            (In(Out(Nothing)) and In(Out(Star))).haveSameTypeRelationAs<In<Out<Nothing>>, In<Out<*>>>()
        }
    }

    @Test
    fun `star projection against Out of Out of AnyQ`() {
        withBuiltTypeAssertions {
            class Out<out T>

            val Out = public type "Out"("out T")


            (Out(Out(AnyQ)) and Out(Out(Star))).haveSameTypeRelationAs<Out<Out<Any?>>, Out<Out<*>>>()
        }
    }

    @Test
    fun `star projection against Out of Out of Foo`() {
        withBuiltTypeAssertions {
            class Foo
            class Out<out T>

            val Foo = public type "Foo"
            val Out = public type "Out"("out T")


            (Out(Out(Foo)) and Out(Out(Star))).haveSameTypeRelationAs<Out<Out<Foo>>, Out<Out<*>>>()
        }
    }

    @Test
    fun `star projection against Out of Out of Nothing`() {
        withBuiltTypeAssertions {
            class Out<out T>

            val Out = public type "Out"("out T")


            (Out(Out(Nothing)) and Out(Out(Star))).haveSameTypeRelationAs<Out<Out<Nothing>>, Out<Out<*>>>()
        }
    }

    @Test
    fun `in exact type`() {
        withBuiltTypeAssertions {
            class Foo
            class Bar

            val Foo = public type "Foo"("T")
            val Bar = public type "Bar"

            (Foo(`in`(Bar)) and Foo(Bar)).haveSameTypeRelationAs<Foo, Foo>()
        }
    }

    @Test
    fun `in exact out type`() {
        withBuiltTypeAssertions {
            class Foo
            class Bar

            val Foo = public type "Foo"("T")
            val Baz = public type "Baz"
            val Bar = public type "Bar"("out T") extends { (T) -> Foo(T) and Baz }

            (Foo(`in`(Bar)) and Foo(out(Bar))).haveSameTypeRelationAs<Foo, Foo>()
        }
    }
}

object TestDoubleTypeBehavior: TypeBehavior<TypeExpression, TypeSymbol, TypeParameterSymbol> {
    override fun getFqnOf(symbol: TypeSymbol): String = symbol.fqName
    override fun getMinimumVisibilityOf(expression: TypeExpression): DeclarationVisibility {
        TODO("Not yet implemented")
    }

    override fun asConcreteType(type: TypeExpression): ConcreteType<TypeSymbol, TypeExpression>? =
        (type as? TypeExpression.Concrete)?.let { ConcreteType(it.symbol, it.arguments) }

    override fun superTypesOfWithoutAny(symbol: TypeSymbol): Sequence<TypeExpression> {
        TODO("Not yet implemented")
    }

    override fun getTypeParameterSymbolsOf(symbol: TypeSymbol): List<TypeParameterSymbol> = symbol.parameters
    override fun isStarProjection(type: TypeExpression): Boolean = type is TypeExpression.StarProjection
    override fun isTopType(type: TypeExpression): Boolean = type is TypeExpression.TopType
    override fun isBottomType(type: TypeExpression): Boolean = type is TypeExpression.BottomType
    override fun asTypeParameterReference(type: TypeExpression): TypeParameterSymbol? =
        (type as? TypeExpression.Generic)?.symbol
    override fun getVarianceOf(typeParameter: TypeParameterSymbol): TypeVariance = typeParameter.variance
    override fun unwrapNullableOrNull(type: TypeExpression): TypeExpression? =
        (type as? TypeExpression.Nullable)?.expression

    override fun unwrapVarianceOrNull(type: TypeExpression): WithVariance<TypeExpression>? =
        (type as? TypeExpression.InType)?.let { WithVariance(TypeVariance.Contravariant, it.expression) }
            ?: (type as? TypeExpression.OutType)?.let { WithVariance(TypeVariance.Covariant, it.expression) }

}

internal inline fun <T> assertEquals(expected: T, actual: T, message: () -> String) {
    if (expected != actual) fail(message())
}

//object KTypeTypeBehavior: TypeBehavior<KTypeProjection, KClass<*>, KTypeParameter> {
//    override fun getFqnOf(symbol: KClass<*>): String = symbol.qualifiedName ?: symbol.simpleName!!
//
//    override fun asConcreteType(type: KTypeProjection): ConcreteType<KClass<*>, KTypeProjection>? =
//        type.type?.let { type ->
//            (type.classifier as? KClass<*>)?.let { symbol ->
//                ConcreteType(symbol, type.arguments)
//            }
//        }
//
//    override fun getDirectSuperTypesOf(symbol: KClass<*>): List<KTypeProjection> =
//        symbol.allSupertypes.map { KTypeProjection(KVariance.OUT, it) }
//
//    override fun getTypeParameterSymbolsOf(symbol: KClass<*>): List<KTypeParameter> {
//        symbol
//    }
//
//    override fun isStarProjection(type: KTypeProjection): Boolean {
//        TODO("Not yet implemented")
//    }
//
//    override fun unwrapNullableOrNull(type: KTypeProjection): KTypeProjection? {
//        TODO("Not yet implemented")
//    }
//
//    override fun isTopType(type: KTypeProjection): Boolean {
//        TODO("Not yet implemented")
//    }
//
//    override fun isBottomType(type: KTypeProjection): Boolean {
//        TODO("Not yet implemented")
//    }
//
//    override fun asGenericIdentifier(type: KTypeProjection): Any? {
//        TODO("Not yet implemented")
//    }
//
//    override fun unwrapVarianceOrNull(type: KTypeProjection): WithVariance<KTypeProjection>? {
//        TODO("Not yet implemented")
//    }
//
//}

fun interface TypeExpressionConverter<TypeLike> {
    fun convert(typeLike: TypeLike): List<TypeExpression>
}

val selfTypeExpressionConverter: TypeExpressionConverter<TypeExpression> =
    TypeExpressionConverter { it -> listOf(it) }

val typeParameterSymbolToTypeExpressionConverter: TypeExpressionConverter<TypeParameterSymbol> =
    TypeExpressionConverter { it -> listOf(TypeExpression.Generic(it)) }

val typeToTypeExpressionConverter: TypeExpressionConverter<TypeSymbol> =
    TypeExpressionConverter { it ->
        assert(it.parameters.isEmpty()) { "Trying to convert $it to type expression without the required type arguments" }
        listOf(TypeExpression.Concrete(it, emptyList()))
    }

private typealias TypeExpressionListBuilderFunction = (List<TypeParameterSymbol>) -> List<TypeExpression>
private typealias TypeExpressionBuilderFunction = (List<TypeParameterSymbol>) -> TypeExpression

context(builder: LanguageTestDouble.ModuleBuilder)
infix fun Visibility.type(name: String): TypeSymbol =
    TypeSymbol(this, name, emptyList(), emptyList(), null).also(builder::registerType)

context(
    converter1: TypeExpressionConverter<T1>,
    converter2: TypeExpressionConverter<T2>,
)
infix fun <T1, T2> T1.and(other: T2): List<TypeExpression> = converter1.convert(this) + converter2.convert(other)

context(builder: LanguageTestDouble.ModuleBuilder)
infix fun Visibility.type(names: NameAndTypeParameterNames): TypeSymbol =
    TypeSymbol(
        this,
        names.name,
        names.typeParameterNames.map {
            TypeParameterSymbol(
                name = it.removePrefix("out ").removePrefix("in "),
                variance = when {
                    it.startsWith("out ") -> TypeVariance.Covariant
                    it.startsWith("in ") -> TypeVariance.Contravariant
                    else -> TypeVariance.Invariant
                },
                ownerOrNullIfNotBound = null,
            )
        },
        emptyList(),
        null
    ).also(builder::registerType)

data class NameAndTypeParameterNames(val name: String, val typeParameterNames: List<String>)

context(builder: LanguageTestDouble.ModuleBuilder)
operator fun String.invoke(vararg generics: String): NameAndTypeParameterNames =
    NameAndTypeParameterNames(this, generics.toList())

context(builder: LanguageTestDouble.ModuleBuilder, converter: TypeExpressionConverter<T1>)
operator fun <T1> TypeSymbol.invoke(a: T1): TypeExpression =
    TypeExpression.Concrete(this, converter.convert(a))

context(
    builder: LanguageTestDouble.ModuleBuilder,
    converter1: TypeExpressionConverter<T1>,
    converter2: TypeExpressionConverter<T2>,
)
operator fun <T1, T2> TypeSymbol.invoke(a: T1, b: T2): TypeExpression =
    TypeExpression.Concrete(this, converter1.convert(a) + converter2.convert(b))

context(
    builder: LanguageTestDouble.ModuleBuilder,
    converter1: TypeExpressionConverter<T1>,
    converter2: TypeExpressionConverter<T2>,
    converter3: TypeExpressionConverter<T3>,
)
operator fun <T1, T2, T3> TypeSymbol.invoke(a: T1, b: T2, c: T3): TypeExpression =
    TypeExpression.Concrete(this, converter1.convert(a) + converter2.convert(b) + converter3.convert(c))

context(builder: LanguageTestDouble.ModuleBuilder)
fun TypeParameterSymbol.expr(): TypeExpression = TypeExpression.Generic(this)

context(builder: LanguageTestDouble.ModuleBuilder, converter: TypeExpressionConverter<TypeLike>)
fun <TypeLike> out(typeLike: TypeLike): TypeExpression = TypeExpression.OutType(converter.convert(typeLike).single())

context(builder: LanguageTestDouble.ModuleBuilder, converter: TypeExpressionConverter<TypeLike>)
fun <TypeLike> `in`(typeLike: TypeLike): TypeExpression = TypeExpression.InType(converter.convert(typeLike).single())

context(builder: LanguageTestDouble.ModuleBuilder)
fun TypeSymbol.swap(old: TypeSymbol): TypeSymbol = also { new ->
    builder.deregisterType(old)
    builder.registerType(new)
}

@OptIn(ExperimentalTypeInference::class)
@OverloadResolutionByLambdaReturnType
@JvmName("extendsList")
context(_: LanguageTestDouble.ModuleBuilder)
infix fun TypeSymbol.extends(superTypesBuilder: TypeExpressionListBuilderFunction): TypeSymbol =
    copy(supertypes = superTypesBuilder(parameters)).swap(this)

@OptIn(ExperimentalTypeInference::class)
@OverloadResolutionByLambdaReturnType
context(_: LanguageTestDouble.ModuleBuilder)
infix fun TypeSymbol.extends(superTypesBuilder: TypeExpressionBuilderFunction): TypeSymbol =
    copy(supertypes = listOf(superTypesBuilder(parameters))).swap(this)

context(_: LanguageTestDouble.ModuleBuilder)
infix fun TypeSymbol.extends(superTypes: List<TypeExpression>): TypeSymbol = copy(supertypes = superTypes).swap(this)

context(builder: LanguageTestDouble.ModuleBuilder, converter: TypeExpressionConverter<TypeLike>)
infix fun <TypeLike> TypeSymbol.extends(superType: TypeLike): TypeSymbol =
    copy(supertypes = converter.convert(superType)).swap(this)

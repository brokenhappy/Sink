package foo.bar

import org.jetbrains.kotlin.compiler.plugin.template.Injectable
import org.jetbrains.kotlin.compiler.plugin.template.InjectionCache

interface Foo
internal interface Bar
interface Baz

@Injectable
private fun foo(): Foo = object: Foo {}

@Injectable
private fun bar(): Bar = object: Bar {}

@Injectable
private fun baz(foo: Foo, bar: Bar): Baz = object: Baz {}

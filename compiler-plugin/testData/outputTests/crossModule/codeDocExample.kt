// MODULE: modulea
// FILE: fooAndBar.kt
package modulea

import org.jetbrains.kotlin.compiler.plugin.template.Injectable

interface Foo
interface Bar
interface Baz
interface Buz

@Injectable
fun foo(): Foo = object : Foo {}

@Injectable
fun bar(foo: Foo, baz: Baz, buz: Buz): Bar = object : Bar {}

// MODULE: moduleb(modulea)
// FILE: bazFoobsAndBarbs.kt
package moduleb

import org.jetbrains.kotlin.compiler.plugin.template.Injectable
import modulea.Foo
import modulea.Bar
import modulea.Baz
import modulea.Buz

interface Foobs
interface Barbs
interface Bazbs


@Injectable
fun baz(foo: Foo, bazbs: Bazbs): Baz = object : Baz {}

@Injectable
fun foobs(foo: Foo): Foobs = object : Foobs {}

@Injectable
fun barbs(bar: Bar, foobs: Foobs, bazbs: Bazbs): Barbs = object : Barbs {}
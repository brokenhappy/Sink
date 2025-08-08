// MODULE: modulea
// FILE: foo.kt
package modulea

import org.jetbrains.kotlin.compiler.plugin.template.Injectable

interface Foo

@Injectable
fun foo(): Foo = object : Foo {}

// MODULE: moduleb(modulea)
// FILE: bar.kt
package moduleb

import org.jetbrains.kotlin.compiler.plugin.template.Injectable
import modulea.Foo

interface Bar

@Injectable
fun bar(foo: Foo): Bar = object : Bar {}

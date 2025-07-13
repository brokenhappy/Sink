// RUN_PIPELINE_TILL: FRONTEND

package foo.bar

import org.jetbrains.kotlin.compiler.plugin.template.Injectable

interface Foo

@Injectable
fun foo(): Foo = object: Foo {}

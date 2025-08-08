// MODULE: modulea
// FILE: foo.kt
package modulea

import com.woutwerkman.sink.Injectable

interface Foo

@Injectable
fun foo(): Foo = object : Foo {}

// MODULE: moduleb(modulea)
// FILE: bar.kt
package moduleb

import com.woutwerkman.sink.Injectable
import modulea.Foo

interface Bar

@Injectable
fun bar(foo: Foo): Bar = object : Bar {}

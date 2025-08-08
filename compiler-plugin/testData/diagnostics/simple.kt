// RUN_PIPELINE_TILL: FRONTEND

package foo.bar

import com.woutwerkman.sink.Injectable

interface Foo

@Injectable
fun foo(): Foo = object: Foo {}

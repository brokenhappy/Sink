package foo.bar

import com.woutwerkman.sink.Injectable
import com.woutwerkman.sink.InjectionCache

interface Foo
internal interface Bar
interface Baz

@Injectable
private fun foo(): Foo = object: Foo {}

@Injectable
private fun bar(): Bar = object: Bar {}

@Injectable
private fun baz(foo: Foo, bar: Bar): Baz = object: Baz {}

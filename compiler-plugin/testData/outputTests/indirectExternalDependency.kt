import com.woutwerkman.sink.Injectable

interface Foo
interface Bar
interface Baz

@Injectable
fun foo(bar: Bar): Foo = object : Foo {}

@Injectable
fun baz(foo: Foo): Baz = object : Baz {}

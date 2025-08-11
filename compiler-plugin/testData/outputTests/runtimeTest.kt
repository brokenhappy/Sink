
import com.woutwerkman.sink.Injectable
import com.woutwerkman.sink.InjectionCache

interface Foo {
    fun getText(): String
}

@Injectable
private fun foo(): Foo = object: Foo {
    override fun getText(): String = "OK"
}

fun box(): String = InjectionCache().get<Foo>().getText()

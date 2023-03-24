package exercises
import org.junit.*
import org.junit.Assert.*
import Lab03.Stream
import Lab03.List.*

class TestStream {

  val s = Stream.take(Stream.iterate(0)(_ + 1))(10)


  @Test def testDrop() =
    assertEquals(Cons(6, Cons(7, Cons(8, Cons(9, Nil())))), Stream.toList(Stream.take(Stream.drop(s)(6))(10)))
    assertEquals(Nil(), Stream.toList(Stream.take(Stream.drop(s)(10))(10)))
    assertEquals(Nil(), Stream.toList(Stream.take(Stream.drop(s)(11))(10)))

  @Test def testCostant() =
    assertEquals(Cons(1, Cons(1, Cons(1, Cons(1, Nil())))), Stream.toList(Stream.take(Stream.constant(1))(4)))
    assertEquals(Cons(2, Cons(2, Cons(2, Cons(2, Nil())))), Stream.toList(Stream.take(Stream.constant(2))(4)))
    assertEquals(Cons(3, Cons(3, Cons(3, Cons(3, Nil())))), Stream.toList(Stream.take(Stream.constant(3))(4)))

  @Test def testFibonacciStreams() =
    val fibs = Stream.fibonacci()
    assertEquals(Cons(0, Cons(1, Cons(1, Cons(2, Cons(3, Cons(5, Cons(8, Cons(13, Cons(21, Cons(34, Nil())))))))))), Stream.toList(Stream.take(fibs)(10)))
    assertEquals(Cons(0, Cons(1, Cons(1, Cons(2, Cons(3, Cons(5, Cons(8, Cons(13, Nil())))))))), Stream.toList(Stream.take(fibs)(8)))
}

package exercises
import exercises.Lab03.List.*
import org.junit.*
import org.junit.Assert.*
import exercises.Lab03.*

class TestList:
  val l: List[Int] = Cons(10, Cons(20, Cons(30, Nil())))
  
  @Test def testDrop(): Unit =
    assertEquals(Cons(10, Cons(20, Cons(30, Nil()))), drop(l, 0))
    assertEquals(Cons(20, Cons(30, Nil())), drop(l, 1))
    assertEquals(Cons(30, Nil()), drop(l, 2))
    assertEquals(Nil(), drop(l, 4))
    assertEquals(Cons(10, Cons(20, Cons(30, Nil()))), drop(l, -1))

  @Test def testAppend(): Unit =
    val tail = Cons(40, Nil())
    assertEquals(Cons(10, Cons(20, Cons(30, Cons(40, Nil())))),append(l, tail))
    assertEquals(l, append(l, Nil()))

  @Test def testFlatMap(): Unit =
    assertEquals(Cons(11, Cons(21, Cons(31, Nil()))), flatMap(l) (v => Cons(v + 1, Nil())))
    assertEquals(Cons(11, Cons(12, Cons(21, Cons(22, Cons(31, Cons(32, Nil())))))), flatMap(l)(v => Cons(v + 1, Cons(v + 2, Nil()))))

  @Test def testMap(): Unit =
    assertEquals(Cons(11, Cons(21, Cons(31, Nil()))), map(l) (v => v + 1))
    assertEquals(Cons(100, Cons(200, Cons(300, Nil()))), map(l) (v => v * 10))

  @Test def testFilter(): Unit =
    assertEquals(Cons(10, Cons(20,(Cons(30, Nil())))), filter(l) (v => v % 10 == 0))
    assertEquals(Cons(30, Nil()), filter(l) (v => v % 10 == 0 && v % 3 == 0))
    
  @Test def testMax(): Unit =
    assertEquals(Some(30), max(l))
    assertEquals(None, max(Nil()))

  @Test def testInvertList() =
    assertEquals(Cons(30, Cons(20, Cons(10, Nil()))), invert(l))

package exercises
import exercises.Lab03.List.*
import org.junit.*
import org.junit.Assert.*

class TestList:
  val l: List[Int] = Cons(10, Cons(20, Cons(30, Nil())))
  
  @Test def testDrop() =
    assertEquals(Cons(20, Cons(30, Nil())), drop(l, 0))
    assertEquals(Cons(10, Cons(30, Nil())), drop(l, 1))
    assertEquals(Cons(10, Cons(20, Nil())), drop(l, 3))
    assertEquals(Cons(10, Cons(20, Cons(30, Nil()))), drop(l, 4))
    assertEquals(Cons(10, Cons(20, Cons(30, Nil()))), drop(l, -1))
    
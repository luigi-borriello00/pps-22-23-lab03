package exercises
import org.junit.*
import org.junit.Assert.*
import Lab03.List.*
import Lab03.*

class TestFold {
  val lst = Cons(3,Cons(7,Cons(1,Cons(5, Nil()))))

  @Test def testFoldLeft() =
    assertEquals(-16, foldLeft(lst)(0)(_ - _))
    assertEquals(16, foldLeft(lst)(0)(_ + _))
    assertEquals(105, foldLeft(lst)(1)(_ * _))

  @Test def testFoldRight() =
    assertEquals(-8, foldRight(lst)(0)(_ - _))
    assertEquals(16, foldRight(lst)(0)(_ + _))
    assertEquals(105, foldRight(lst)(1)(_ * _))


}

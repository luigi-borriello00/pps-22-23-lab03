package exercises
import exercises.Lab03.List.*
import org.junit.*
import org.junit.Assert.*
import exercises.Lab03.*
import exercises.Lab03.Person.{Student, Teacher, getCourses}

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

class TestFold {
  val lst: List[Int] = Cons(3,Cons(7,Cons(1,Cons(5, Nil()))))

  @Test def testFoldLeft(): Unit =
    assertEquals(-16, foldLeft(lst)(0)(_ - _))
    assertEquals(16, foldLeft(lst)(0)(_ + _))
    assertEquals(105, foldLeft(lst)(1)(_ * _))

  @Test def testFoldRight(): Unit =
    assertEquals(-8, foldRight(lst)(0)(_ - _))
    assertEquals(16, foldRight(lst)(0)(_ + _))
    assertEquals(105, foldRight(lst)(1)(_ * _))

    assertEquals(-8, foldRight2(lst)(0)(_ - _))
    assertEquals(16, foldRight2(lst)(0)(_ + _))
    assertEquals(105, foldRight2(lst)(1)(_ * _))

}

class TestTeachers {

  @Test def testGetCourses(): Unit =
    // list of person
    val personList: List[Person] = Cons(Student("Kelvin Olaya", 1), Cons(Teacher("Alezzoli Massandro", "Clean Coding"), Nil()))
    assertEquals(Cons("Clean Coding", Nil()), getCourses(personList))

}


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
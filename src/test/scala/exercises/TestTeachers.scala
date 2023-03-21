package exercises
import org.junit.*
import org.junit.Assert.*
import exercises.Lab03.*
import exercises.Lab03.List.*

import exercises.Lab03.Person.*

class TestTeachers {

  @Test def testGetCourses(): Unit =
    // list of person
    val personList: List[Person] = Cons(Student("Kelvin Olaya", 1), Cons(Teacher("Alezzoli Massandro", "Clean Coding"), Nil()))
    assertEquals(Cons("Clean Coding", Nil()), getCourses(personList))

}

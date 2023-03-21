package exercises

object Lab03 extends App:
  enum List[E]:
    case Cons(head: E, tail: List[E])
    case Nil()

  object List:
    // 1a
    def drop[A](l: List[A], n: Int): List[A] = (l, n) match
      case(Cons(h, t), n) if n > 0 => drop(t, n-1)
      case _ => l

    // 1b
    def append[A](left: List[A], right: List[A]): List[A] = (left, right) match
      case (Nil(), _) => right
      case(Cons(h, t), _) => Cons(h, append(t, right))

    // 1c
    def flatMap[A,B](l: List[A])(f: A => List[B]): List[B] = (l, f) match
      case(Nil(), _) => Nil()
      case(Cons(h, t), f) => append(f(h), flatMap(t)(f))

    // 1d
    def map[A, B](l: List[A])(f: A => B): List[B] =
      flatMap(l)(e => Cons(f(e), Nil()))

    def filter[A](l: List[A])(p: A => Boolean): List[A] =
      flatMap(l)(
        e => p(e) match
        case true => Cons(e, Nil())
        case false => Nil()
      )

    // 2
    def max(l: List[Int]): Option[Int] = l match
      case Nil() => None
      case(Cons(h, t)) => max(t) match
        case None => Some(h)
        case Some(e) => Some(Integer.max(h, e))

  // 3
  enum Person:
    case Student(name: String, year: Int)
    case Teacher(name: String, course: String)

  object Person:
    import exercises.Lab03.List.*
      def name(p: Person): String = p match
        case Student(n, _) => n
        case Teacher(n, _) => n

      def getCourses(l: List[Person]): List[String] =
        flatMap(l)(e => e match
          case Teacher(_, c) => Cons(c, Nil())
          case _ => Nil()
        )

  // 4
  import List.*

  def foldLeft[A, B](l: List[A])(d: B)(f: (B, A) => B): B = l match
    case Nil() => d
    case Cons(h, t) => foldLeft(t)(f(d, h))(f)

  def foldRight[A, B](l: List[A])(d: B)(f: (A, B) => B): B = l match
    case Nil() => d
    case Cons(h, t) => f(h, foldRight(t)(d)(f))














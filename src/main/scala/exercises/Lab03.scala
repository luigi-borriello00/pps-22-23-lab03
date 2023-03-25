package exercises

import u03.Lists.List
import u03.Streams.Stream
import u03.Streams.Stream.{Cons, Empty}

import scala.annotation.tailrec
import scala.language.postfixOps

object Lab03 extends App:
  enum List[E]:
    case Cons(head: E, tail: List[E])
    case Nil()

  object List:
    // 1a
    @tailrec
    def drop[A](l: List[A], n: Int): List[A] = (l, n) match
      case (Cons(h, t), n) if n > 0 => drop(t, n-1)
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

    def invert[A](l: List[A]): List[A] = l match
      case Nil() => Nil()
      case Cons(h, t) => append(invert(t), Cons(h, Nil()))

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

  @tailrec
  def foldLeft[A, B](l: List[A])(d: B)(f: (B, A) => B): B = l match
    case Nil() => d
    case Cons(h, t) => foldLeft(t)(f(d, h))(f)


  def foldRight[A, B](l: List[A])(d: B)(f: (A, B) => B): B =
    foldLeft(invert(l))(d)((b, a) => f(a, b))

  
  def foldRight2[A, B](l: List[A])(d: B)(f: (A, B) => B): B = l match
    case Nil() => d
    case Cons(h, t) => f(h, foldRight(t)(d)(f)) 

  // 5
  enum Stream[A]:
    private case Empty()
    private case Cons(head: () => A, tail: () => Stream[A])
  object Stream:

    def empty[A](): Stream[A] = Empty()

    def cons[A](hd: => A, tl: => Stream[A]): Stream[A] =
      lazy val head = hd
      lazy val tail = tl
      Cons(() => head, () => tail)

    def toList[A](stream: Stream[A]): List[A] = stream match
      case Cons(h, t) => List.Cons(h(), toList(t()))
      case _ => List.Nil()

    def map[A, B](stream: Stream[A])(f: A => B): Stream[B] = stream match
      case Cons(head, tail) => cons(f(head()), map(tail())(f))
      case _ => Empty()

    def filter[A](stream: Stream[A])(pred: A => Boolean): Stream[A] = stream match
      case Cons(head, tail) if (pred(head())) => cons(head(), filter(tail())(pred))
      case Cons(head, tail) => filter(tail())(pred)
      case _ => Empty()

    def take[A](stream: Stream[A])(n: Int): Stream[A] = (stream, n) match
      case (Cons(head, tail), n) if n > 0 => cons(head(), take(tail())(n - 1))
      case _ => Empty()

    def iterate[A](init: => A)(next: A => A): Stream[A] =
      cons(init, iterate(next(init))(next))

    @tailrec
    def drop[A](stream: Stream[A])(n:Int): Stream[A] = (stream, n) match
      case (Cons(_, t), n) if n > 0 => drop(t())(n-1)
      case _ => stream

    def constant[A](k: A): Stream[A] =
      iterate(k)(x => x)


    // fibonacci infinite stream
    def fibonacci(): Stream[Int] =
      def fib(a: Int, b: Int): Stream[Int] =
        cons(a, fib(b, a + b))
      fib(0, 1)


  end Stream













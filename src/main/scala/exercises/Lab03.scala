package exercises

object Lab03 extends App:
  enum List[E]:
    case Cons(head: E, tail: List[E])
    case Nil()

  object List:
    @scala.annotation.tailrec
    def drop[A](l: List[A], n: Int): List[A] = (l, n) match
      case(l, 0) => l
      case(l, i) if i < 0 => l
      case (Cons(_, t), i) => drop(t, i-1)
      case _ => Nil()

    def append[A](left: List[A], right: List[A]): List[A] = (left, right) match
      case (Nil(), _) => right
      case(Cons(h, t), _) => Cons(h, append(t, right))

    def flatMap[A,B](l: List[A])(f: A => List[B]): List[B] = (l, f) match
      case(Nil(), _) => Nil()
      case(Cons(h, t), f) => append(f(h), flatMap(t)(f))







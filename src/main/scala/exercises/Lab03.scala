package exercises

object Lab03 extends App:
  enum List[E]:
    case Cons(head: E, tail: List[E])
    case Nil()

  object List:
    @scala.annotation.tailrec
    def drop[A](l: List[A], n: Int): List[A] = (l, n) match
      case (Nil(), _) => l
      case (l, n) if n < 0 => l
      case (Cons(_, t), 0) => t
      case (Cons(_, t), i) => drop(t, i-1)


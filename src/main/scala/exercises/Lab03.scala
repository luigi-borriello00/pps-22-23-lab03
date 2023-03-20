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



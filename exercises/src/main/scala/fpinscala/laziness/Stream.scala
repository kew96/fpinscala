package fpinscala.laziness

import Stream._

trait Stream[+A] {

  def toList: List[A] = {
    this match {
      case Empty => List.empty[A]
      case Cons(h, t) => h() :: t().toList
    }
  }

  def foldRight[B](z: => B)(f: (A, => B) => B): B = // The arrow `=>` in front of the argument type `B` means that the function `f` takes its second argument by name and may choose not to evaluate it.
    this match {
      case Cons(h, t) => f(h(), t().foldRight(z)(f)) // If `f` doesn't evaluate its second argument, the recursion never occurs.
      case _ => z
    }

  def exists(p: A => Boolean): Boolean =
    foldRight(false)((a, b) => p(a) || b) // Here `b` is the unevaluated recursive step that folds the tail of the stream. If `p(a)` returns `true`, `b` will never be evaluated and the computation terminates early.

  @annotation.tailrec
  final def find(f: A => Boolean): Option[A] = this match {
    case Empty => None
    case Cons(h, t) => if (f(h())) Some(h()) else t().find(f)
  }

  def take(n: Int): Stream[A] = {
    this match {
      case Cons(h, t) if n > 0 => cons(h(), t() take (n - 1))
      case Cons(h, _) => cons(h(), empty)
      case _ => empty
    }
  }

  def drop(n: Int): Stream[A] = {
    if (n == 0) this
    else this match {
      case Cons(_, t) => t() drop (n - 1)
      case _ => empty
    }
  }

  def takeWhile(p: A => Boolean): Stream[A] = {
    this match {
      case Cons(h, t) if p(h()) => cons(h(), t() takeWhile p)
      case _ => empty
    }
  }

  def takeWhile2(p: A => Boolean): Stream[A] = {
    foldRight(empty[A])((a, b) =>
      if (p(a)) cons(a, b)
      else empty
    )
  }

  def forAll(p: A => Boolean): Boolean = {
    foldRight(true)((a, b) => p(a) && b)
  }

  def headOption: Option[A] = {
    foldRight(None: Option[A])((a, _) => Some(a))
  }

  // 5.7 map, filter, append, flatmap using foldRight. Part of the exercise is
  // writing your own function signatures.

  def map[B](f: A => B): Stream[B] = {
    foldRight(empty[B])((a, b) => cons(f(a), b))
  }

  def filter(p: A => Boolean): Stream[A] = {
    foldRight(empty[A])((a, b) =>
      if (p(a)) cons(a, b)
      else b
    )
  }

  def append[B >: A](e: => Stream[B]): Stream[B] = {
    foldRight(e)((a, b) => cons(a, b))
  }

  def flatMap[B](f: A => Stream[B]): Stream[B] = {
    foldRight(empty[B])((a, b) => f(a) append b)
  }

  def map2[B](f: A => B): Stream[B] = {
    unfold(this)(
      {
        case Cons(h, t) => Some((f(h()), t()))
        case _ => None
      }
    )
  }

  def take2(n: Int): Stream[A] = {
    unfold((this, n))(
      {
        case (Cons(h, t), n) if n > 0 => Some((h(), (t(), n - 1)))
        case _ => None
      }
    )
  }

  def takeWhile3(p: A => Boolean): Stream[A] = {
    unfold(this)(
      {
        case Cons(h, t) if p(h()) => Some((h(), t()))
        case _ => None
      }
    )
  }

  def zipWith[B, C](b: Stream[B])(f: (A, B) => C): Stream[C] = {
    unfold((this, b))(
      {
        case (Cons(h1, t1), Cons(h2, t2)) => Some((f(h1(), h2()), (t1(), t2())))
        case _ => None
      }
    )
  }

  def zipAll[B](b: Stream[B]): Stream[(Option[A], Option[B])] =
    zipWithAll(b)((_, _))

  def zipWithAll[B, C](b: Stream[B])(f: (Option[A], Option[B]) => C): Stream[C] =
    Stream.unfold((this, b)) {
      case (Cons(h1, t1), Cons(h2, t2)) => Some((f(Some(h1()), Some(h2())), (t1(), t2())))
      case (Cons(h, t), Empty) => Some((f(Some(h()), Option.empty[B]), (t(), empty[B])))
      case (Empty, Cons(h, t)) => Some((f(Option.empty[A], Some(h())), (empty[A], t())))
      case (Empty, Empty) => None
    }

  def startsWith[B](s: Stream[B]): Boolean = {
    zipAll(s) takeWhile2 (_._2.exists(
      {
        case Empty => false
        case _ => true
      }
    )
      ) forAll
      {
        case (h1, h2) => h1 == h2
      }
  }

  def tails: Stream[Stream[A]] = {
    unfold(this)(
      {
        case Cons(_, t) => Some((this, t()))
        case _ => None
      }
    ) append Stream(Stream.empty[A])
  }

  def hasSubsequence[B](b: Stream[B]): Boolean ={
    tails exists(_ startsWith b)
  }

  def scanRight[B](z: B)(f: (A, => B) => B): Stream[B] = {
    foldRight((z, Stream(z)))((a, p0) =>
    {
      lazy val p1 = p0
      val b2 = f(a, p1._1)
      (b2, cons(b2, p1._2))
    }
    )._2
  }
}

case object Empty extends Stream[Nothing]

case class Cons[+A](h: () => A, t: () => Stream[A]) extends Stream[A]

object Stream {
  def cons[A](hd: => A, tl: => Stream[A]): Stream[A] = {
    lazy val head = hd
    lazy val tail = tl
    Cons(() => head, () => tail)
  }

  def empty[A]: Stream[A] = Empty

  def apply[A](as: A*): Stream[A] =
    if (as.isEmpty) empty
    else cons(as.head, apply(as.tail: _*))

  val ones: Stream[Int] = cons(1, ones)

  def constant[A](e: A): Stream[A] = {
    lazy val tail: Stream[A] = Cons(() => e, () => tail)
    tail
  }

  def fibs(n1: Int = 0, n2: Int = 1): Stream[Int] = {
    cons(n1, fibs(n1, n1 + n2))
  }

  def from(n: Int): Stream[Int] = cons(n, from(n + 1))

  def unfold[A, S](z: S)(f: S => Option[(A, S)]): Stream[A] = {
    f(z) match {
      case Some((v, e)) => cons(v, unfold(e)(f))
      case _ => Stream.empty[A]
    }
  }

  val fibs2: Stream[Int] = {
    unfold((0, 1))({ case (n1, n2) => Some(n1, (n1, n1 + n2)) })
  }

  def from2(n: Int): Stream[Int] = {
    unfold(n)(n => Some((n, n + 1)))
  }

  def constant2[A](e: A): Stream[A] = {
    unfold(e)(e => Some((e, e)))
  }

  def ones2: Stream[Int] = constant2(1)
}
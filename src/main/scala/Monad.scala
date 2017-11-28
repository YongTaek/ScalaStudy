import Par.Par

import language.higherKinds

trait Functor[F[_]] {

  def map[A, B](fa: F[A])(f: A => B):F[B]

  def distribute[A, B](fab: F[(A, B)]): (F[A], F[B]) =
    (map(fab)(_._1), map(fab)(_._2))

  def codistribute[A, B](e: Either[F[A], F[B]]): F[Either[A, B]] =
    e match {
      case Left(fa) => map(fa)(Left(_))
      case Right(fb) => map(fb)(Right(_))
    }

}

trait Monad[F[_]] extends Functor[F] {
  def unit[A](a: => A): F[A]
  def flatMap[A, B](ma: F[A])(f: A => F[B]): F[B]

  def map[A,B](ma: F[A])(f: A => B): F[B] =
    flatMap(ma)(a => unit(f(a)))

  def map2[A, B, C](ma: F[A], mb: F[B])(f: (A, B) => C): F[C] =
    flatMap(ma)(a => map(mb)(b => f(a, b)))

}

object Monad {

  val ParMonad = new Monad[Par] {
    override def unit[A](a: => A) = Par.unit(a)

    override def flatMap[A, B](ma: Par[A])(f: A => Par[B]) = Par.flatMap(ma)(f)
  }

  val ParserMonad = new Monad[Parsers] {
    override def unit[A](a: => A) = Parsers.unit(a)

    override def flatMap[A, B](ma: Parsers[A])(f: A => Parsers[B]) =
      Parsers.flatMap(ma)(f)
  }

  val OptionMonad = new Monad[Option] {
    override def unit[A](a: => A) = Some(a)

    override def flatMap[A, B](ma: Option[A])(f: A => Option[B]) =
      if (ma.isEmpty) None else f(ma.get)
  }

  val ListMonad = new Monad[List] {
    override def unit[A](a: => A) = List(a)

    override def flatMap[A, B](ma: List[A])(f: A => List[B]) = ma.flatMap(f)
  }

  val StreamMonad = new Monad[Stream] {
    override def unit[A](a: => A) = Stream(a)

    override def flatMap[A, B](ma: Stream[A])(f: A => Stream[B]) = ma.flatMap(f)
  }

  case class State[S, A](run: S => (A, S)) {
    def flatMap[B](f: A => State[S, B]): Monad.State[S, B] = ???

  }

  val stateMonad = new Monad[({type l[k] = State[Int, k]})#l] {
    override def unit[A](a: => A) = State(s => (a, s))

    override def flatMap[A, B](ma: State[Int, A])(f: A => State[Int, B]) = ma.flatMap(f)
  }


  

}

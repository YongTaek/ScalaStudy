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

  def sequence[A](lma: List[F[A]]): F[List[A]] =
    traverse(lma)(identity)

  def traverse[A, B](la: List[A])(f: A => F[B]): F[List[B]] =
    la.foldRight(unit(List.empty[B]))((a, acc) => map2(f(a), acc)(_ :: _))

  def replicateM[A](n: Int, ma: F[A]): F[List[A]] =
    sequence(List.fill(n)(ma))



}

object Monad {

  val ParMonad = new Monad[Par] {
    override def unit[A](a: => A) = Par.unit(a)

    override def flatMap[A, B](ma: Par[A])(f: A => Par[B]) = Par.flatMap(ma)(f)
  }

//  val ParserMonad = new Monad[Parsers] {
//    override def unit[A](a: => A) = Parsers.unit(a)
//
//    override def flatMap[A, B](ma: Parsers[A])(f: A => Parsers[B]) =
//      Parsers.flatMap(ma)(f)
//  }

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
//
//  val stateMonad = new Monad[({type l[k] = State[Int, k]})#l] {
//    override def unit[A](a: => A) = State(s => (a, s))
//
//    override def flatMap[A, B](ma: State[Int, A])(f: A => State[Int, B]) = ma.flatMap(f)
//  }

  /*
  OptionMonad.replicateM(3, Some(1))을 하면 결과로
  Some(List(1,1,1))이 나온다. 때문에 Option에서 replicateM은 단순히 n만큼의 갯수를 가진 List에 대한 Option을 만드는 것과 같다.

  하지만 ListMonad.replicateM(3, List(1,2))의 경우, 결과로
  List(List(1,1,1), List(1,1,2), List(1,2,1), List(1,2,2), List(2,1,1), List(2,1,2), List(2,2,1), List(2,2,2)) 이 나온다.
  살펴보면 List of List를 만드는 것이며 안쪽 List의 경우 갯수가 n과 같은 3이다. 그리고 그 값의 경우
  1,1,1부터 2,2,2까지 인자로 넣어준 List(1,2)의 값인 1,2의 모든 경우의 수가 나온다는 것을 알 수 있다.
  List(1,2)의 값인 1,2 2개로 만들어내는 사이즈가 3인 리스트의 경우의 수 와 같다. 따라서 2 ** 3 으로 8개의 List를 가진 List이다.
  이걸 적용 시켜서 ListMonad.replicate(2, List(1,2,3))을 구하면  3 ** 2인 9개의 List를 가진 List가 나올 것이고,
  9개의 List는 1,2,3으로 만들 수 있는 사이즈가 2인 List의 모든 경우의 수일 것이다.
  실제 결과로는
  List(List(1, 1), List(1, 2), List(1, 3), List(2, 1), List(2, 2), List(2, 3), List(3, 1), List(3, 2), List(3, 3))이 나온다.
   */

  


}

object Main extends App {
  import Monad._

  println(OptionMonad.replicateM(3, Some(1)))
  println(ListMonad.replicateM(2, List(1,2,3)))
}
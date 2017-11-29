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

  def compose[A,B,C](f: A => F[B], g: B => F[C]): A => F[C] =
    a => flatMap(f(a))(g)

  def flatMapViaCompose[A,B](ma: F[A])(f: A => F[B]): F[B] =
    compose((_:Unit) => ma, f)(())

  def join[A](mma: F[F[A]]): F[A] =
    flatMap(mma)(identity)

  def flatMapViaJoinAndMap[A,B](ma: F[A])(f: A => F[B]): F[B] =
    join(map(ma)(f))

  def compose[A,B,C](f: A => F[B], g: B => F[C]): A => F[C] =
    a => join(map(f(a))(g))



  def sequence[A](lma: List[F[A]]): F[List[A]] =
    traverse(lma)(identity)

  def traverse[A, B](la: List[A])(f: A => F[B]): F[List[B]] =
    la.foldRight(unit(List.empty[B]))((a, acc) => map2(f(a), acc)(_ :: _))

  def replicateM[A](n: Int, ma: F[A]): F[List[A]] =
    sequence(List.fill(n)(ma))

  def filterM[A](ms: List[A])(f: A => F[Boolean]): F[List[A]] =
    ms.foldRight(unit(List.empty[A]))((a, acc) => flatMap(f(a))(x => addListIfTrue(x)(unit(a), acc)))

  def addListIfTrue[A](b: Boolean)(a: F[A], list: F[List[A]]): F[List[A]] =
    if (b) map2(a, list)(_ :: _)
    else list

  /*
  compose(f, unit) == f
  compose(unit, f) == f

  1. compose(f, unit) == f
  flatMap(f(a))(unit)
  => f(a)
  2. compose(unit, f)
  flatMap(unit(a))(f)
  => f(unit(a))
  => f(a)
   */

  /*
  compose(compose(f, g), h) == compose(f, compose(g, h))
  a => join(map(f(a))(g))

  1. compose(compose(f, g), h)
  join(map(compose(f, g))(h))
  => join(map(join(map(f)(g)))(h))
  => join(join(map(f)(g)).map(h))
  => join(join(f.map(g)).map(h))
  => join(f.map(g)).map(h).join()
  => f.map(g).join().map(h).join()

  2. compose(f, compose(g, h))
  join(map(f)(compose(g, h)))
  => join(map(f)(join(map(g)(h))))
  => join(f.map(join(map(g)(h))))
  => join(f.map(join(g.map(h))))
  => f.map(join(g.map(h))).join()
  => f.map(g.map(h).join()).join()

   */


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

  /*
  Par 에서 결합법칙의 의미.
  Par에서 flatMap이 가지는 의미는 첫 번째 인수의 결과를 가지고 두 번쨰 인수에 대해 적용시키는 것이다. 즉 3개의 thread를 결합하는 상황에 있다면 어짜피 안쪽부터 순서대로 실행될 것이기 때문에
  1,2를 결합한 뒤 3을 결합하는 것이나 2,3을 결합한뒤 1과 결합하는 것이나 같은 의미라는 것이다.
   */

  case class Id[A](value: A) {
    def map[B](f: A => B):Id[B] = Id(f(value))

    def flatMap[B](f: (A) => Id[B]) = f(value)
  }

  val IdMonad = new Monad[Id] {
    override def unit[A](a: => A) = Id(a)

    override def flatMap[A, B](ma: Id[A])(f: (A) => Id[B]) = ma flatMap f
  }

  case class State[S, A](run: S => (A, S)) {
    def map[B](f: A => B): State[S, B] =
      State(s => {
        val (a, s1) = run(s)
        (f(a), s1)
      })
    def flatMap[B](f: A => State[S, B]): State[S, B] =
      State(s => {
        val (a, s1) = run(s)
        f(a).run(s1)
      })
  }

  type IntState[A] = State[Int, A]

  object IntStateMonad extends Monad[IntState] {
    override def unit[A](a: => A): IntState[A] = State(s => (a, s))

    override def flatMap[A, B](ma: IntState[A])(f: (A) => IntState[B]): IntState[B] = ma flatMap f
  }

  def stateMonad[S] = new Monad[({type f[x] = State[S, x]})#f] {
    override def unit[A](a: => A) = State(s => (a, s))

    override def flatMap[A, B](ma: State[S, A])(f: (A) => State[S, B]) = ma flatMap f
  }

  

}

object Main extends App {
  import Monad._

  println(OptionMonad.replicateM(3, Some(1)))
  println(ListMonad.replicateM(2, List(1,2,3)))

  // 11.11
  def F:Int => Option[Int] =
    (a:Int) => Some(a + 3)

  println(OptionMonad.compose(F, (a:Int) => OptionMonad.unit(a))(1))
  println(F(1))


}
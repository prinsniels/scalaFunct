package chapt_4

sealed trait Either[+E, +A] {
  def map[B](f: A => B): Either[E, B] = {
    this match {
      case Left(e) => Left(e)
      case Right(x) => Right(f(x))
    }
  }

  def flatMap[EE >: E, B](f: A => Either[EE, B]): Either[EE, B] = {
    this match {
      case Left(e) => Left(e)
      case Right(x) => f(x)  // note that this can return a left or a right
    }
  }

  def orElse[EE >: E,B >: A](b: => Either[EE, B]): Either[EE, B] = {
    this match {
      case Left(_) => b
      case Right(x) => Right(x)
    }
  }

  def map2[EE >: E, B, C](b: Either[EE, B])(f: (A, B) => C): Either[EE, C] = {
    for {
      a <- this       // flatMap this
      bb <- b         // map this
    } yield f(a, bb)  // execute function

    // this should be the same, but this does not work.... it pisses of the type checking
    // this.flatMap(a => b.map(bb => f(a, bb)))
  }

}

case class Left[+E](value: E) extends Either[E, Nothing] // E for Error
case class Right[+A](value: A) extends Either[Nothing, A] // A for Accept

object Either {


  def Try[A](a: => A): Either[Exception, A] = {
    try Right(a)
    catch {
      case e: Exception => Left(e)
    }
  }

  // 4.7
  def sequence[E, A](es: List[Either[E, A]]): Either[E, List[A]] =
    traverse(es)(x => x)

  // 4.7
  def traverse[E, A, B](xs: List[A])(f: A => Either[E, B]): Either[E, List[B]] =
    xs match {
      case Nil => Right(Nil)
      case h::t => (f(h) map2 traverse(t)(f))(_ :: _)
  }
}

object test {

  def mean(xs: IndexedSeq[Double]): Either[String, Double] = {
    if (xs.isEmpty)
      Left("mean of empty list")
    else
      Right(xs.sum / xs.length)
  }

  def main(args: Array[String]): Unit = {
    println("--either tests--")
  }
}
package chapt_3

import scala.annotation.tailrec

sealed trait List[+A]

case object Nil extends List[Nothing]

case class Cons[+A](h: A, t: List[A]) extends List[A]

object List {

  /**
    * takes a list and returns the tail of the list
    */
  def tail[A](list: List[A]): List[A] = {
    list match {
      case Nil => Nil
      case Cons(_, t) => t
    }
  }

  def setHead[A](l: List[A], v: A): List[A] = {
    l match {
      case Nil => Cons(v, Nil)
      case Cons(_, t) => Cons(v, t)
    }
  }

  @tailrec
  def drop[A](l: List[A], n: Int): List[A] =
    if (n <= 0) l
    else l match {
      case Nil => Nil
      case Cons(_, t) => drop(t, n - 1)
    }

  @tailrec
  def dropWhile[A](l: List[A], f: A => Boolean): List[A] =
    l match {
      case Nil => Nil
      case Cons(h, t) if f(h) => dropWhile(t, f)
      case _ => l
    }

  /**
    * Note that this definition only copies values until the first list is exhausted, so its run-
    * time and memory usage are determined only by the length of a1 . The remaining list
    * then just points to a2 . If we were to implement this same function for two arrays, we’d
    * be forced to copy all the elements in both arrays into the result. In this case, the
    * immutable linked list is much more efficient than an array!
    *
    */
  def append[A](a1: List[A], a2: List[A]): List[A] =
    a1 match {
      case Nil => a2
      case Cons(h, t) => Cons(h, append(t, a2))
    }


  /**
    * note the copying of the list....
    */
  def init[A](l: List[A]): List[A] =
    l match {
      case Nil => Nil
      case Cons(_, Nil) => Nil
      case Cons(h, t) => Cons(h, init(t))
    }


  def foldRight[A, B](l: List[A], z: B)(f: (A, B) => B): B =
    l match {
      case Nil => z
      case Cons(h, t) => f(h, foldRight(t, z)(f))
    }

  @tailrec
  def foldLeft[A, B](l: List[A], z: B)(f: (B, A) => B): B =
    l match {
      case Nil => z
      case Cons(h, t) => foldLeft(t, f(z, h))(f)
    }

  def sumL(l: List[Int]): Int =
    foldLeft(l, 0)(_ + _)


  def apply[A](as: A*): List[A] =
    if (as.isEmpty) Nil
    else Cons(as.head, apply(as.tail: _*))


  def length[A](l: List[A]): Int =
    foldRight(l, 0)((_, acc) => acc + 1)

  def productL(l: List[Double]): Double =
    foldLeft(l, 1.0)(_ * _)

  def lengthL[A](l: List[A]): Int =
    foldLeft(l, 0)((acc, h) => acc + 1)

  /**
    * Input should take list[A]() as z value
    * when given Nil the interpreter does not understand what is going on.
   */
  def reverseL[A](l: List[A]): List[A] =
    foldLeft(l, List[A]())((a, b) => Cons(b, a))


  /**
    * Via reverse is a common trick
    */
  def foldRightViaFoldLeft[A,B](l: List[A], z: B)(f: (A,B) => B): B =
    foldLeft(reverseL(l), z)((b,a) => f(a,b))

  /**
    * the z of foldleft is a function which takes a b and returns a b
    * the result is given as start of the next iteration, effectively creating a reverse
    */
  def foldRightViaFoldLeft_1[A,B](l: List[A], z: B)(f: (A,B) => B): B =
    foldLeft(l, (b:B) => b)((g,a) => b => g(f(a,b)))(z)

  def foldLeftViaFoldRight[A,B](l: List[A], z: B)(f: (B,A) => B): B =
    foldRight(l, (b:B) => b)((a,g) => b => g(f(b,a)))(z)


  def appendR[A](a: List[A], b: List[A]): List[A] =
    foldRight(a, b)(Cons(_,_))


  // 3.16
  def addOne(l: List[Int]): List[Int] =
    foldRight(l, Nil:List[Int])((h,t) => Cons(h+1,t))

  // 3.17
  def doubleToString(l: List[Double]): List[String] =
    foldRight(l, Nil:List[String])((h,t) => Cons(h.toString(),t))

  // 3.18
//  def map[A,B](as: List[A])(f: A => B): List[B] =
//    foldLeft(as, List[B])

}


object tests {

  def main(args: Array[String]): Unit = {
    val work: List[Int] = List(1, 2, 3)
    val test: List[Int] = List(4,5,6)

    println(List.appendR(work, test))
    println(List.foldRight(List(1,2,3),0)(_-_))
    println(List.foldLeft(List(1,2,3),0)(_-_))
  }
}

package chapt_4

sealed trait Option[+A] {
  // placing functions here allows us to call option.function instead of function(option)
  /**
    * Apply function, if Some, else None
    */
  def map[B](f: A => B): Option[B] = {
    this match {
      case Some(x) => Some(f(x))
      case None => None
    }
  }

  /**
    * Apply a function to the option that might fail, so returns an option. Return the resulting
    */
  def flatMap[B](f: A => Option[B]): Option[B] = {
    this.map(f).getOrElse(None)
  }

  //The B >: A says that the B type
  //parameter must be
  //a supertype of A .
  /**
    * Get the value if it is there, else return the default
    */
  def getOrElse[B >: A](default: => B): B = {
    this match {
      case None => default
      case Some(x) => x
    }
  }

  def orElse[B >: A](ob: => Option[B]): Option[B] = {
    this map (Some(_)) getOrElse ob
  }

  def filter(f: A => Boolean): Option[A] = {
    this match {
      case Some(a) if f(a) => this
      case _ => None
    }
  }
}

case class Some[A](get: A) extends Option[A]

case object None extends Option[Nothing]


object Option {

  def map2[A,B,C](a: Option[A], b: Option[B])(f: (A, B) => C): Option[C] = {
    /**
      * 1. take a and map (we use FlatMap for if further calculations get killed)
      * 2. take b and map (note that the map does not start if aa is None
      * 3. Effectively it stops when aa or bb is None, else it will return C
      */
    a.flatMap(aa => b.map(bb => f(aa, bb)))
  }

  // 4.4
  /**
    * Note how the function exits when a flatmap return None when called on None
    */
  def sequence[A](a: List[Option[A]]): Option[List[A]] = {
    a match {
      case Nil => Some(Nil)
      case h :: t => h.flatMap(hh => sequence(t).map(hh :: _))
    }
  }

  // 4.5
  def traverse[A, B](a: List[A])(f: A => Option[B]): Option[List[B]] = {
    a match {
      case Nil => Some(Nil)
      case h::t => map2(f(h), traverse(t)(f))(_ :: _)
    }
  }

}


object ensurenceExample {
  /**
    * Top secret formula for computing an annual car
    * insurance premium from two key factors.
    */
  def insuranceRateQuote(age: Int, numberOfSpeedingTickets: Int): Double = ???


  /**
    * custom try function that takes a peace of code to try
    * When it fails we get None else we get the option back
   */
  def Try[A](a: => A): Option[A] = {
    try Some(a)
    catch {
      case _: Exception => None
    }
  }

  def parseInsuranceRateQuote(
                               age: String,
                               numberSpeedingTickets: String
                             ): Option[Double] = {
    val optAge: Option[Int] = Try(age.toInt)
    val optTickets: Option[Int] = Try(numberSpeedingTickets.toInt)

    Option.map2(optAge, optTickets)(insuranceRateQuote)
  }
}

object tests {

  def mean(xs: Seq[Double]): Option[Double] = {
    if (xs.isEmpty) None
    else Some(xs.sum / xs.length)
  }

  def variance(xs: Seq[Double]): Option[Double] = {
    /*
     1. take the mean of xs -> Option[Double]
     when it is None, flatMap is not started
     2. take m as the mean and input it into the function
     3. use a flatMap, because it can result in a Option, so Option within a Option
     Take the option out of it
     */
    mean(xs) flatMap (m => mean(xs.map(x => math.pow(x - m, 2))))
  }

  def lift[A, B](f: A => B): Option[A] => Option[B] = _ map f

  def caps(s: String): String = {
    s.toUpperCase()
  }

  def main(args: Array[String]): Unit = {
    println("-- kicking it --")

    println(caps("hallo"))

    println(Option.sequence(List(Some(1), Some(2), None, Some(4))))
    println(Option.sequence(List(Some(1), Some(2), Some(3), Some(4))))
  }
}
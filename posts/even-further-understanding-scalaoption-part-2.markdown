---
comments: true
date: 2010-09-01 15:45:27
layout: post
slug: even-further-understanding-scalaoption-part-2
title: Even Further Understanding scala.Option (part 2)
wordpressid: 812
tags: Programming
---

As a follow-on to [Further Understanding scala.Option](http://blog.tmorris.net/further-understanding-scalaoption/), following are another 10 exercises (numbered 16 to 25). Included are solutions to the original 1 to 15 exercises. Instructions are in the comments.


    
    
    // Scala version 2.8.0.final
    // http://scala-tools.org/repo-releases/org/scala-tools/testing/scalacheck_2.8.0/1.7/scalacheck_2.8.0-1.7.jar
    
~~~{.Scala}
/*

  PART 1
  ======
  Below are 15 exercises numbered 1 to 15. The task is to emulate the scala.Option API
  without using Some/None subtypes, but instead using a fold (called a
  catamorphism).

  A couple of functions are already done (map, get)
  to be used as an example. ScalaCheck tests are given below to
  verify the work. The desired result is to have all tests passing.

  The 15th exercise is not available in the existing Scala API so
  instructions are given in the comments.


  Part 2
  ======

  Below are 10 exercises numbered 16 to 25. The task is to implement additional
  methods for the Optional data type. These methods are not provided in the
  scala.Option API so to determine the correct result requires reading the method
  type signature and ensuring that the tests pass.

  The 25th exercise is notable in that its signature says nothing about
  scala.Option yet it is usable for Option (see the test for example).


  Revision History
  ================

  23/08/2010
  * Initial revision

  ----------------

  23/08/2010
  * Fixed prop_getOrElse. Thanks Michael Bayne.

  ----------------

  26/08/2010
  * Add lazy annotation to orElse method.

  ----------------

  01/09/2010
  Added Part 2

  02/09/2010
  * Fixed mapOptionals test (why wasn't it failing?). Thanks Alec Zorab.
  * Added comments including *** special note ***

*/


trait Optional[A] {
  // single abstract method
  def fold[X](some: A => X, none: => X): X

  import Optional._

  // Done for you.
  def map[B](f: A => B): Optional[B] =
    fold(f andThen some, none[B])

  // Done for you.
  // WARNING: undefined for None
  def get: A =
    fold(a => a, error("None.get"))

  // Exercise 1
  def flatMap[B](f: A => Optional[B]): Optional[B] =
    fold(f, none)

  // Exercise 2
  // Rewrite map but use flatMap, not fold.
  def mapAgain[B](f: A => B): Optional[B] =
    flatMap(f andThen some)

  // Exercise 3
  def getOrElse(e: => A): A =
    fold(s => s, e)

  // Exercise 4
  def filter(p: A => Boolean): Optional[A] =
    fold(a => if(p(a)) some(a) else none, none)

  // Exercise 5
  def exists(p: A => Boolean): Boolean =
    fold(p, false)

  // Exercise 6
  def forall(p: A => Boolean): Boolean =
    fold(p, true)

  // Exercise 7
  def foreach(f: A => Unit): Unit =
    fold(f, ())

  // Exercise 8
  def isDefined: Boolean =
    fold(_ => true, false)

  // Exercise 9
  def isEmpty: Boolean =
    fold(_ => false, true)

  // Exercise 10
  def orElse(o: => Optional[A]): Optional[A] =
    fold(_ => this, o)

  // Exercise 11
  def toLeft[X](right: => X): Either[A, X] =
    fold(Left(_), Right(right))

  // Exercise 12
  def toRight[X](left: => X): Either[X, A] =
    fold(Right(_), Left(left))

  // Exercise 13
  def toList: List[A] =
    fold(List(_), Nil)

  // Exercise 14
  def iterator: Iterator[A] =
    fold(Iterator.single(_), Iterator.empty)

  // Exercise 15 The Clincher!
  // Return a none value if either this or the argument is none.
  // Otherwise apply the function to the argument in some.
  // Don't be afraid to use functions you have written.
  // Better style, more points!
  def applic[B](f: Optional[A => B]): Optional[B] =
    f flatMap map

  // Utility
  def toOption: Option[A] = fold(Some(_), None)

  // Utility
  override def toString =
    fold("some[" + _ + "]", "none")

  // Utility
  override def equals(o: Any) =
    o.isInstanceOf[Optional[_]] && {
      val q = o.asInstanceOf[Optional[_]]
      fold(a => q.exists(a == _),
           q.isEmpty)
    }
}

object Optional {
  // Done for you
  def none[A]: Optional[A] = new Optional[A] {
    def fold[X](some: A => X, none: => X) = none
  }

  // Done for you
  def some[A](a: A): Optional[A] = new Optional[A] {
    def fold[X](some: A => X, none: => X) = some(a)
  }

  // Utility
  def fromOption[A](o: Option[A]): Optional[A] = o match {
    case None    => none
    case Some(a) => some(a)
  }

  // *** Special note ***
  // Some of these functions are likely to be familiar List functions,
  // but with one specific distinction: in every covariant value appearing in
  // the type signature, this value is wrapped in Optional.
  // For example, the unwrapped:
  // filter:          (A => Boolean) => List[A] => List[A]
  // and the wrapped:
  // filterOptionals: (A => Optional[Boolean]) => List[A] => Optional[List[A]]
  //
  // There are other functions of a similar nature below.

  // Exercise 16
  // If a none is encountered, then return a none, otherwise,
  // accumulate all the values in Optional.
  def mapOptionals[A, B](f: A => Optional[B], a: List[A]): Optional[List[B]] =
    error("todo")

  // Exercise 17
  // If a none is encountered, then return a none, otherwise,
  // accumulate all the values in Optional.
  def sequenceOptionals[A](a: List[Optional[A]]): Optional[List[A]] =
    error("todo")

  // Exercise 18
  // Use sequenceOptionals
  def mapOptionalsAgain[A, B](f: A => Optional[B], a: List[A]): Optional[List[B]] =
    error("todo")

  // Exercise 19
  // Use mapOptionals
  def sequenceOptionalsAgain[A](a: List[Optional[A]]): Optional[List[A]] =
    error("todo")

  // Exercise 20
  // If a none is encountered, return none, otherwise,
  // flatten/join by one level.
  def joinOptionals[A](a: Optional[Optional[A]]): Optional[A] =
    error("todo")

  // Exercise 21
  def filterOptionals[A](p: A => Optional[Boolean], a: List[A]): Optional[List[A]] =
    error("todo")

  // Exercise 22
  def fillOptionals[A](n: Int, a: Optional[A]): Optional[List[A]] =
    error("todo")

  // Exercise 23
  // Use sequenceOptionals
  def fillOptionalsAgain[A](n: Int, a: Optional[A]): Optional[List[A]] =
    error("todo")

  // Exercise 24
  // Methods mentioning Optional in the type signature are prohibited, except applic and map
  def mapOptionalsYetAgain[A, B](f: A => Optional[B], a: List[A]): Optional[List[B]] =
    error("todo")

  // Consider: def joinOptional[A](a: Optional[Optional[A]]): Optional[A]
  // This function "flattens" the Optional into a Some value if possible.
  // It is not possible to write this using only applic and map (try it!).

  // Bye bye Option-specificity!
  // (setting up for Exercise 25)
  trait Applic[F[_]] {
    def point[A](a: A): F[A]
    def applic[A, B](f: F[A => B], a: F[A]): F[B]

    final def map[A, B](f: A => B, a: F[A]): F[B] =
      applic(point(f), a)
  }

  object Applic {
    implicit val OptionalApplic: Applic[Optional] = new Applic[Optional] {
      def point[A](a: A): Optional[A] = some(a)
      def applic[A, B](f: Optional[A => B], a: Optional[A]): Optional[B] = a applic f
    }
  }

  // Exercise 25
  // The Double-Clincher!
  def mapWhatever[A, B, F[_]](f: A => F[B], a: List[A])(implicit z: Applic[F]): F[List[B]] =
    error("todo")
}

import org.scalacheck._
import Arbitrary.arbitrary
import Prop._

object TestOptional extends Properties("Optional") {
  import Optional._

  implicit def ArbitraryOptional[A](implicit a: Arbitrary[A]): Arbitrary[Optional[A]] =
    Arbitrary(arbitrary[Option[A]] map fromOption)

  property("map") = forAll ((o: Optional[Int], f: Int => String) =>
    (o map f).toOption == (o.toOption map f))

  property("get") = forAll((o: Optional[Int]) =>
    o.isDefined ==>
      (o.get == o.toOption.get))

  property("flatMap") = forAll((o: Optional[Int], f: Int => Optional[String]) =>
    (o flatMap f).toOption == (o.toOption flatMap (f(_).toOption)))

  property("mapAgain") = forAll ((o: Optional[Int], f: Int => String) =>
    (o mapAgain f).toOption == (o map f).toOption)

  property("getOrElse") = forAll ((o: Optional[Int], n: Int) =>
    (o getOrElse n) == (o.toOption getOrElse n))

  property("filter") = forAll ((o: Optional[Int], f: Int => Boolean) =>
    (o filter f).toOption == (o.toOption filter f))

  property("exists") = forAll ((o: Optional[Int], f: Int => Boolean) =>
    (o exists f) == (o.toOption exists f))

  property("forall") = forAll ((o: Optional[Int], f: Int => Boolean) =>
    (o forall f) == (o.toOption forall f))

  property("foreach") = forAll ((o: Optional[Int], f: Int => Unit, n: Int) => {
    var x: Int = n
    var y: Int = x

    o foreach (t => x = x + t)
    o.toOption foreach (t => y = y + t)

    x == y
  })

  property("isDefined") = forAll ((o: Optional[Int]) =>
    (o.isDefined) == (o.toOption.isDefined))

  property("isEmpty") = forAll ((o: Optional[Int]) =>
    o.isEmpty == o.toOption.isEmpty)

  property("orElse") = forAll ((o: Optional[Int], p: Optional[Int]) =>
    (o orElse p).toOption == (o.toOption orElse p.toOption))

  property("toLeft") = forAll ((o: Optional[Int], n: Int) =>
    (o toLeft n) == (o.toOption toLeft n))

  property("toRight") = forAll ((o: Optional[Int], n: Int) =>
    (o toRight n) == (o.toOption toRight n))

  property("toList") = forAll ((o: Optional[Int]) =>
    o.toList == o.toOption.toList)

  property("iterator") = forAll ((o: Optional[Int]) =>
    o.iterator sameElements o.toOption.iterator)

  // *** READ THIS COMMENT FIRST ***
  // Note that scala.Option has no such equivalent to this method
  // Therefore, reading this test may give away clues to how it might be solved.
  // If you do not wish to spoil it, look away now and follow the
  // instruction in the Exercise comment.
  property("applic") = forAll ((o: Optional[Int => String], p: Optional[Int]) =>
    (p applic o).toOption ==
    (for(f <- o.toOption;
         n <- p.toOption)
    yield f(n)))

  def trace[A](a: A) = {
    println(a)
    a
  }

  property("mapOptionals") = forAll((f: Int => Optional[String], o: List[Int]) =>
  {
    val i = o map f
    mapOptionals(f, o) == (if(i forall (_.isDefined)) some(i map (_.get)) else none)
  })

  property("sequenceOptionals") = forAll((o: List[Optional[String]]) =>
      sequenceOptionals(o) == (if(o exists (_.isEmpty)) none else some(o map (_.get))))

  property("mapOptionalsAgain") = forAll((f: Int => Optional[String], o: List[Int]) =>
      mapOptionalsAgain(f, o) == mapOptionals(f, o))

  property("sequenceOptionalsAgain") = forAll((o: List[Optional[String]]) =>
      sequenceOptionalsAgain(o) == sequenceOptionals(o))

  property("joinOptionals") = forAll((o: Optional[Optional[String]]) =>
      joinOptionals(o) == (if(o.isDefined && o.get.isDefined) o.get else none))

  property("filterOptionals") = forAll((f: Int => Optional[Boolean], o: List[Int]) =>
      filterOptionals(f, o) == (if(o exists (f(_).isEmpty)) none else some(o filter (f(_).get))))

  property("fillOptionals") = forAll((n: Int, o: Optional[String]) =>
      (n < 1000) ==> // prevent stack consumption
      (fillOptionals(n, o) == (if(n <= 0) some(Nil) else (o map (List.fill(n)(_))))))

  property("fillOptionalsAgain") = forAll((n: Int, o: Optional[String]) =>
      (n < 1000) ==> // prevent stack consumption
      (fillOptionalsAgain(n, o) == fillOptionals(n, o)))

  property("mapOptionalsYetAgain") = forAll((f: Int => Optional[String], o: List[Int]) =>
      mapOptionalsYetAgain(f, o) == mapOptionals(f, o))

  property("mapWhatever") = forAll((f: Int => Optional[String], o: List[Int]) =>
      mapWhatever(f, o) == mapOptionals(f, o))

  /*
  $ scala -classpath .:scalacheck_2.8.0-1.7.jar TestOptional
  + Optional.map: OK, passed 100 tests.
  + Optional.get: OK, passed 100 tests.
  + Optional.flatMap: OK, passed 100 tests.
  + Optional.mapAgain: OK, passed 100 tests.
  + Optional.getOrElse: OK, passed 100 tests.
  + Optional.filter: OK, passed 100 tests.
  + Optional.exists: OK, passed 100 tests.
  + Optional.forall: OK, passed 100 tests.
  + Optional.foreach: OK, passed 100 tests.
  + Optional.isDefined: OK, passed 100 tests.
  + Optional.isEmpty: OK, passed 100 tests.
  + Optional.orElse: OK, passed 100 tests.
  + Optional.toLeft: OK, passed 100 tests.
  + Optional.toRight: OK, passed 100 tests.
  + Optional.toList: OK, passed 100 tests.
  + Optional.iterator: OK, passed 100 tests.
  + Optional.applic: OK, passed 100 tests.
  + Optional.mapOptionals: OK, passed 100 tests.
  + Optional.sequenceOptionals: OK, passed 100 tests.
  + Optional.mapOptionalsAgain: OK, passed 100 tests.
  + Optional.sequenceOptionalsAgain: OK, passed 100 tests.
  + Optional.joinOptionals: OK, passed 100 tests.
  + Optional.filterOptionals: OK, passed 100 tests.
  + Optional.fillOptionals: OK, passed 100 tests.
  + Optional.fillOptionalsAgain: OK, passed 100 tests.
  + Optional.mapOptionalsYetAgain: OK, passed 100 tests.
  + Optional.mapWhatever: OK, passed 100 tests.
  */
}
~~~

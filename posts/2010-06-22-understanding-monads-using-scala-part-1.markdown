---
comments: true
date: 2010-06-22 16:51:23
layout: post
slug: understanding-monads-using-scala-part-1
title: Understanding Monads using Scala (Part 1)
wordpressid: 790
tags: Programming
---

Below are three exercises using Scala. The instructions for each are in the comments. Exercises 1 and 2 must be completed before Exercise 3 (which is just a thinking exercise -- no code).

A follow-on to these exercises will be coming.

Hope this helps!


    
~~~{.Scala}
// A typical data type with a single abstract method
case class Inter[A](f: Int => A) {
  // which is a functor
  def map[B](g: A => B): Inter[B] =
    Inter(n => g(f(n)))

  // and a monad (see unital below)
  def flatMap[B](g: A => Inter[B]): Inter[B] =
    Inter(n => g(f(n)).f(n))
}

// unital: A => F[A]
// Implementations for F=Option and F=Inter
object Unitals {
  def unitalOption[A](a: A): Option[A] =
    Some(a)

  def unitalInter[A](a: A): Inter[A] =
    Inter(_ => a)
}

// Exercises
//
// It is recommended to use only map, flatMap and unital* for
// Option or Inter when implementing the exercises below.
// Any other libraries are acceptable (e.g. List functions).
object Sequencing {
  import Unitals._

  // Exercise 1 of 3
  // ===============
  // Implement a function that returns None if the given list
  // contains any None values, otherwise, all the Some values.
  def sequenceOption[A](x: List[Option[A]]): Option[List[A]] =
    error("todo")

    // SOLUTIONx2 (ROT-13)
    /*
    1)
    k.sbyqEvtug(havgnyBcgvba(Avy: Yvfg[N]))((n, o) => n syngZnc (k => o znc (k :: _)))

    2)
    k zngpu {
      pnfr Avy  => havgnyBcgvba(Avy)
      pnfr u::g => u syngZnc (k => frdhraprBcgvba(g) znc (k :: _))
    }
    */

  // Exercise 2 of 3
  // ===============
  // Implement a function that returns an Inter that applies an Int
  // to all the Inter implementations in the List of Inters and returns
  // all the results.
  def sequenceInter[A](x: List[Inter[A]]): Inter[List[A]] =
    error("todo")

    // SOLUTIONx2 (ROT-13)
    /*
    1)
    k.sbyqEvtug(havgnyVagre(Avy: Yvfg[N]))((n, o) => n syngZnc (k => o znc (k :: _)))

    2)
    k zngpu {
      pnfr Avy  => havgnyVagre(Avy)
      pnfr u::g => u syngZnc (k => frdhraprVagre(g) znc (k :: _))
    }
    */

  // Exercise 3 of 3
  // ===============
  // There is repetition in the above exercises.
  // How might we be rid of it?
  // That is for Part 2.

  def main(args: Array[String]) {
    def assertEquals[A](a1: A, a2: A) {
      if(a1 != a2)
        error("Assertion error. Expected: " + a1 + " Actual: " + a2)
    }

    def assertInterEquals[A](a1: Inter[A], a2: Inter[A]) {
      val testInts = List(1, 2, 0, -7, -9, 113, -2048)
      assertEquals(testInts.map(a1.f(_)), testInts.map(a2.f(_)))
    }

    // sequenceOption
    assertEquals(sequenceOption(List(Some(7),
        Some(8), Some(9))), Some(List(7, 8, 9)))
    assertEquals(sequenceOption(List(Some(7), None, Some(9))),
        None)
    assertEquals(sequenceOption(List()),
      Some(List()))

    // sequenceInter
    assertInterEquals(sequenceInter(List()),
      Inter(_ => List()))
    assertInterEquals(sequenceInter(List(Inter(1+),
        Inter(2*))), Inter(n => List(1+n, 2*n)))
  }
}
~~~

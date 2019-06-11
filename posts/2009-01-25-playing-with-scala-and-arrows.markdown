---
comments: true
date: 2009-01-25 12:34:12
layout: post
slug: playing-with-scala-and-arrows
title: Playing with Scala and Arrows
wordpressid: 526
tags: Programming
---

I decided to play around with [arrows](http://www.cs.chalmers.se/~rjmh/pubs.htm) and Scala to see how far I could get. I remember vividly the effort required to get a Monad abstraction for [Scalaz](http://code.google.com/p/scalaz/) to work and be useful all at once with so many competing tensions (e.g. type inferencer, composability, repetition).

I didn't go so far as to test the usefulness, but I was able to express the Kleisli arrow without too much effort. I wonder if it is a viable addition for Scalaz.

Here it is.

The typical Functor/Monad abstraction. These are required for the Kleisli arrow.

    
~~~{.Scala}
trait Functor[F[+_]] {
  def fmap[A, B](fa: F[A], f: A => B): F[B]
}

trait Monad[M[+_]] extends Functor[M] {
  def pure[A](a: A): M[A]
  def bind[A, B](ma: M[A], f: A => M[B]): M[B]

  final def fmap[A, B](fa: M[A], f: A => B) =
    bind(fa, (a: A) => pure(f(a)))
}
~~~



The Kleisli data type and a convenient constructor.

    
~~~{.Scala}
trait Kleisli[M[+_], -A, +B] {
  def apply(a: A): M[B]
}

object Kleisli {
  def kleisli[M[+_], A, B](f: A => M[B]) = new Kleisli[M, A, B] {
    def apply(a: A) = f(a)
  }
}
~~~



The arrow abstraction.

    
~~~{.Scala}
trait Arrow[A[-_, +_]] {
  def arrow[B, C](f: B => C): A[B, C]

  def compose[B, C, D](a1: A[B, C], a2: A[C, D]): A[B, D]

  def first[B, C, D](a: A[B, C]): A[(B, D), (C, D)]

  def second[B, C, D](a: A[B, C]): A[(D, B), (D, C)]
}
~~~



and the clincher...

    
    
~~~{.Scala}
object Arrow {
  val Function1Arrow = new Arrow[Function1] {
    def arrow[B, C](f: B => C) = f

    def compose[B, C, D](a1: B => C, a2: C => D) =
      a2 compose a1

    def first[B, C, D](a: B => C) =
      (bd: (B, D)) => (a(bd._1), bd._2)

    def second[B, C, D](a: B => C) =
      (db: (D, B)) => (db._1, a(db._2))
  }

  def KleisliArrow[M[+_]](implicit m: Monad[M]) =
        new Arrow[PartialType[Kleisli, M]#Apply] {
    import Kleisli.kleisli

    def arrow[B, C](f: B => C) =
      kleisli[M, B, C](b => m.pure(f(b)))

    def compose[B, C, D](a1: Kleisli[M, B, C], a2: Kleisli[M, C, D]) =
      kleisli[M, B, D](b => m.bind(a1(b), (c: C) => a2(c)))

    def first[B, C, D](a: Kleisli[M, B, C]) =
      kleisli[M, (B, D), (C, D)] { case (b, d) => m.fmap(a(b), (c: C) => (c, d)) }

    def second[B, C, D](a: Kleisli[M, B, C]) =
      kleisli[M, (D, B), (D, C)]{ case (d, b) => m.fmap(a(b), (c: C) => (d, c)) }
  }
}
~~~



TODO




  1. Write functions across arrows.


  2. Can arrows be made useful within the constraints of Scala?



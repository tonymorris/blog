---
comments: true
date: 2008-06-26 10:57:10
layout: post
slug: monad-laws-using-reductio-scala
title: Monad Laws using Reductio (Scala)
wordpressid: 91
tags: Programming
---

In the spirit of yesterday's post that denoted the Functor Laws using Reductio, I will also give the Monad Laws. Before I do however, here is an example use of the `FunctorLaws` object that runs 600 unit tests when testing a `Functor` implementation for `scala.Option[A]`, `scala.List` and finally, for the `scala.Either[A, _]` functor just to make it a bit quirky and probably confuse a few of you (in that case, ignore it for now!) :)


    
~~~{.Scala}
val prop_OptionIdentity = identity[Option, Int]
val prop_OptionComposition = composition[Option, Int, String, Long]

val prop_ListIdentity = identity[List, Int]
val prop_ListComposition = composition[List, Int, String, Long]

val prop_EitherIdentity = identity[Apply1Of2[Either, String]#Apply, Int]
val prop_EitherComposition = composition[Apply1Of2[Either, String]#Apply, Int, String, Long]

// OK passed 100 tests. (appears 6 times -- once per property)
~~~



...and the obligatory Monad Laws follow (import statements omitted this time, but they are the same as previous)...


    
~~~{.Scala}
object MonadLaws {
  def leftIdentity[M[_], A, B](implicit m: Monad[M],
                                     am: Arbitrary[M[B]],
                                     aa: Arbitrary[A],
                                     ca: Coarbitrary[A]) =
    prop((a: A, f: A => M[B]) =>
      m.bind(f, m.unit(a)) === f(a))

  def rightIdentity[M[_], A](implicit m: Monad[M],
                                      am: Arbitrary[M[A]]) =
    prop((ma: M[A]) => m.bind((a: A) =>
      m.unit(a), ma) === ma)

  def associativity[M[_], A, B, C](implicit m: Monad[M],
                                           am: Arbitrary[M[A]],
                                           ca: Coarbitrary[A],
                                           cb: Coarbitrary[B],
                                           amb: Arbitrary[M[B]],
                                           amc: Arbitrary[M[C]]) =
    prop((ma: M[A], f: A => M[B], g: B => M[C]) =>
      m.bind(g, m.bind(f, ma)) === m.bind((a: A) => m.bind(g, f(a)), ma))
}
~~~



Woot! Woot!

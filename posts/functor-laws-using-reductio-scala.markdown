---
comments: true
date: 2008-06-25 16:07:08
layout: post
slug: functor-laws-using-reductio-scala
title: Functor Laws using Reductio (Scala)
wordpressid: 90
tags: Programming
---

A somewhat intricate and extremely useful code snippet using [Reductio](http://reductiotest.org/) for testing the laws of any Functor instance (note that the `Functor` type is part of [Scalaz](http://wiki.workingmouse.com/index.php/Scalaz)):


    
~~~{.Scala}
import reductios.Property._
import reductios.Arbitrary
import reductio.Coarbitrary
import reductios.Arbitrary._

object FunctorLaws {
  def identity[F[_], A](implicit f: Functor[F], af: Arbitrary[F[A]]) =
    prop((fa: F[A]) => f.fmap((a: A) => a, fa) === fa)

  def composition[F[_], A, B, C](implicit f: Functor[F],
                                          af: Arbitrary[F[A]],
                                          ac: Arbitrary[C],
                                          cb: Coarbitrary[B],
                                          ab: Arbitrary[B],
                                          ca: Coarbitrary[A]) =
    prop((fa: F[A], h: B => C, i: A => B) =>
            f.fmap(h compose i, fa) === f.fmap(h, f.fmap(i, fa)))
}
~~~

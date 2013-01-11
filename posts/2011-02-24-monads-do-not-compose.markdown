---
comments: true
date: 2011-02-24 11:32:16
layout: post
slug: monads-do-not-compose
title: Monads do not compose
wordpressid: 938
tags: Programming
---

On the Scala mailing list, I said "monads do not compose." What does this mean exactly? Hopefully the following answers it.

Let us first state the `Functor`, `Applicative` and `Monad` interfaces:


    
~~~{.Scala}
trait Functor[F[_]] {
  def fmap[A, B](f: A => B, a: F[A]): F[B]
}

trait Applicative[F[_]] extends Functor[F] {
  def ap[A, B](f: F[A => B], a: F[A]): F[B]
  def point[A](a: A): F[A]
  override final def fmap[A, B](f: A => B, a: F[A]) =
    ap(point(f), a)
}

trait Monad[F[_]] extends Applicative[F] {
  def flatMap[A, B](f: A => F[B], a: F[A]): F[B]
  override final def ap[A, B](f: F[A => B], a: F[A]) =
    flatMap((ff: A => B) => fmap((aa: A) => ff(aa), a), f)
}
~~~



Let's now restate our claims:




  1. Functors compose


  2. Applicatives compose


  3. Monads do not compose



I will now address points 1) and 2). What does it mean for X to compose in this context? Let us now answer that question more succinctly:

It means that we can write a function with this signature (for some X):


    
~~~{.Scala}
def XCompose[M[_], N[_]](implicit mx: X[M], nx: X[N]):
  X[({type λ[α]=M[N[α]]})#λ]
~~~



This return type may look like gobbledy, but it's just a fancy way of doing partial type constructor application in Scala. It is essentially the X type constructor applied to the composition of M and N and followed by a type variable just like M and N itself (kind * -> *). Let us now test our claims.

Functors compose. That is to say, we can write:

    
~~~{.Scala}
def FunctorCompose[M[_], N[_]]
    (implicit mx: Functor[M], nx: Functor[N]):
       Functor[({type λ[α]=M[N[α]]})#λ]
~~~



Let's do it!


    
~~~{.Scala}
new Functor[({type λ[α]=M[N[α]]})#λ] {
  def fmap[A, B](f: A => B, a: M[N[A]]) =
      mx.fmap((na: N[A]) => nx.fmap(f, na), a)
}
~~~



Here we are composing two Functors. Claim 1) is demonstrated. What about claim 2)? Here we go. Hold your breath!


    
~~~{.Scala}
def ApplicativeCompose[M[_], N[_]]
    (implicit ma: Applicative[M], na: Applicative[N]):
        Applicative[({type λ[α]=M[N[α]]})#λ] =
  new Applicative[({type λ[α]=M[N[α]]})#λ] {
  def ap[A, B](f: M[N[A => B]], a: M[N[A]]) = {
    def liftA2[X, Y, Z](f: X => Y => Z, a: M[X], b: M[Y]): M[Z] =
      ma.ap(ma.fmap(f, a), b)
    liftA2((ff: N[A => B]) => (aa: N[A]) => na.ap(ff, aa), f, a)
  }
  def point[A](a: A) =
    ma point (na point a)
}
~~~



Have fun untangling that! It's a bit noisy but this is mostly because Scala requires a lot of boilerplate. Hopefully you can get to the essence of what it means to compose two Applicative functors.

Now let's move to Monad. Essentially, I am saying that you won't be able to write such a function for Monad. I really encourage you to try to write it so you can see the twist that you will get into.


    
~~~{.Scala}
def MonadCompose[M[_], N[_]](implicit ma: Monad[M], na: Monad[N]):
    Monad[({type λ[α]=M[N[α]]})#λ]
~~~



Here is a compilable version
[http://paste.pocoo.org/show/343606/](http://paste.pocoo.org/show/343606/)

Hope that helps!

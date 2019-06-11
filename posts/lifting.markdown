---
comments: true
date: 2011-07-17 10:57:29
layout: post
slug: lifting
title: Lifting
wordpressid: 1025
tags: Programming
---

Below is a compileable Scala source file. If you read it from top to bottom, it may help with some insights regarding applicative functors. It was partially inspired by Eric's rendition of [The Essence of the Iterator Pattern](http://etorreborre.blogspot.com/2011/06/essence-of-iterator-pattern.html).


~~~ {.Scala}
trait Lift[F[_]] {
  // Spot the pattern in these type signatures
  // of increasing arity.

  def lift0[A]:
    A => F[A]

  def lift1[A, B]:
    (A => B) => (F[A] => F[B])

  def lift2[A, B, C]:
    (A => B => C) => (F[A] => F[B] => F[C])

  def lift3[A, B, C, D]:
    (A => B => C => D) => (F[A] => F[B] => F[C] => F[D])

  // ... and so on

  // The relationship between lift<n> and lift<n-1>
  // can be given by a function,

  def ap[A, B]:
    F[A => B] => (F[A] => F[B])
}

trait LiftImpl[F[_]] extends Lift[F] {
  // Each lift function uses
  // the previous lift function and ap.

  def lift1[A, B]:
    (A => B) => (F[A] => F[B])
    = ap compose lift0

  def lift2[A, B, C]:
    (A => B => C) => (F[A] => F[B] => F[C])
    = f => ap compose lift1(f)

  def lift3[A, B, C, D]:
    (A => B => C => D) => (F[A] => F[B] => F[C] => F[D])
    = f => a => ap compose lift2(f)(a)
}

// Notes
// * lift0 is often called: unit, return, pure, point, η
// * lift1 is often called: fmap, map, ∘
// * lift<n> is often called: liftA<n>, liftM<n>
~~~




All that is left to do is to implement the `LiftImpl` trait! You can do this by implementing the `ap` and `lift0` functions.

Examples of implementations that I know will work out if you try to implement them:

* `class ListLift extends LiftImpl[List]`{.Scala}


* `class OptionLift extends LiftImpl[Option]`{.Scala}


* `class EitherLift[R] extends LiftImpl[({type λ[α] = Either[R, α]})#λ]`{.Scala}


* `class Function1Lift[R] extends LiftImpl[({type λ[α] = R => α})#λ]`{.Scala}


Those last couple are a bit funky, but a lot of that is syntax noise rather than anything too complicated. Fill out the body of those classes!

---
comments: true
date: 2008-09-10 13:36:45
layout: post
slug: 20-intermediate-scala-exercises
title: 20 Intermediate Scala Exercises
wordpressid: 317
tags: Programming
---

I will be publishing answers to these exercises as well as the [Revised Scala Exercises](http://blog.tmorris.net/revised-scala-exercises/) in the near future. In the meantime, see if you can tackle the exercises below. In almost all cases, the type signature is enough to derive what the implementation should be. In all other cases, the solution is as difficult to derive anyway, so consider it an equivalent achievement.

The source file below compiles, but is missing certain function implementations (where you see `error("todo")`). Enjoy and if you have questions, please feel free to post them :)


    
~~~{.Scala}
trait PartialType[T[_, _], A] {
  type Apply[B] = T[A, B]
  type Flip[B] = T[B, A]
}

trait Fluffy[F[_]] {
  def furry[A, B](f: A => B, fa: F[A]): F[B]
}

object Fluffy {
  // Exercise 1
  // Relative Difficulty: 1
  def ListFluffy: Fluffy[List] = error("todo")

  // Exercise 2
  // Relative Difficulty: 1
  def OptionFluffy: Fluffy[Option] = error("todo")

  // Exercise 3
  // Relative Difficulty: 1
  def StreamFluffy: Fluffy[Stream] = error("todo")

  // Exercise 4
  // Relative Difficulty: 1
  def ArrayFluffy: Fluffy[Array] = error("todo")

  // Exercise 5
  // Relative Difficulty: 5
  def Function1Fluffy[X]: Fluffy[PartialType[Function1, X]#Apply] =
    error("todo")

  // Exercise 6
  // Relative Difficulty: 6
  def EitherLeftFluffy[X]: Fluffy[PartialType[Either.LeftProjection, X]#Flip] =
    error("todo")

  // Exercise 7
  // Relative Difficulty: 4
  def EitherRightFluffy[X]: Fluffy[PartialType[Either.RightProjection, X]#Apply] =
    error("todo")
}

trait Misty[M[_]] extends Fluffy[M] {
  def banana[A, B](f: A => M[B], ma: M[A]): M[B]

  def unicorn[A](a: A): M[A]

  // Exercise 8
  // Relative Difficulty: 3
  // (use banana and/or unicorn)
  def furry[A, B](f: A => B, ma: M[A]): M[B] = error("todo")
}

object Misty {
  // Exercise 9
  // Relative Difficulty: 2
  def ListMisty: Misty[List] = error("todo")

  // Exercise 10
  // Relative Difficulty: 2
  def OptionMisty: Misty[Option] = error("todo")

  // Exercise 11
  // Relative Difficulty: 2
  def StreamMisty: Misty[Stream] = error("todo")

  // Exercise 12
  // Relative Difficulty: 2
  def ArrayMisty: Misty[Array] = error("todo")

  // Exercise 13
  // Relative Difficulty: 6
  def Function1Misty[X]: Misty[PartialType[Function1, X]#Apply] =
    error("todo")

  // Exercise 14
  // Relative Difficulty: 7
  def EitherLeftMisty[X]: Misty[PartialType[Either.LeftProjection, X]#Flip] =
    error("todo")

  // Exercise 15
  // Relative Difficulty: 5
  def EitherRightMisty[X]: Misty[PartialType[Either.RightProjection, X]#Apply] =
    error("todo")

  // Exercise 16
  // Relative Difficulty: 3
  def jellybean[M[_], A](ma: M[M[A]], m: Misty[M]): M[A] = error("todo")

  // Exercise 17
  // Relative Difficulty: 6
  def apple[M[_], A, B](ma: M[A], mf: M[A => B], m: Misty[M]): M[B] =
    error("todo")

  // Exercise 18
  // Relative Difficulty: 6
  def moppy[M[_], A, B](as: List[A], f: A => M[B], m: Misty[M]): M[List[B]] =
    error("todo")
}

object AdvancedFun {
  case class State[S, A](f: S => (S, A))

  // Exercise 19
  // Relative Difficulty: 9
  def StateFluffy[S]: Fluffy[PartialType[State, S]#Apply] = error("todo")

  // Exercise 20
  // Relative Difficulty: 10
  def StateMisty[S]: Misty[PartialType[State, S]#Apply] = error("todo")
}
~~~

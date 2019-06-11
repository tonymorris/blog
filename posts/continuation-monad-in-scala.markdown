---
comments: true
date: 2008-12-28 13:25:41
layout: post
slug: continuation-monad-in-scala
title: Continuation monad in Scala
wordpressid: 512
tags: Programming
tag: Scala
---

Here is how a continuation data type might look in Scala:


    
~~~{.Scala}
sealed trait Continuation[R, +A] {
  def apply(f: A => R): R

  import Continuation.continuation

  def map[B](k: A => B) =
    continuation[R, B](z => apply(z compose k))

  def flatMap[B](k: A => Continuation[R, B]) =
    continuation[R, B](z => apply(k(_)(z)))
}
~~~



Note that there are `map` and `flatMap` functions that satisfy the functor and monad laws. `Continuation[R, _]` is a monad.

Now let's add some construction functions:


    
    
~~~{.Scala}
object Continuation {
  def continuation[R, A](g: (A => R) => R) = new Continuation[R, A] {
    def apply(f: A => R) = g(f)
  }

  def unit[R] = new {
    def apply[A](a: A) = continuation[R, A](f => f(a))
  }

  def callcc[R, A, B](f: (A => Continuation[R, B]) => Continuation[R, A]) =
    continuation[R, A](k => f(a => continuation(x => k(a)))(k))
}
~~~



The `continuation` function is a straight-forward construction. The `unit` function constructs a continuation is such a way as to satisfy the monad laws against the `flatMap` method. The `callcc` function (call with current continuation) is to allow "escaping" from the current continuation.

Here is some example client code that squares a number using a continuation (see `squarec`):

    
~~~{.Scala}
object Square {
  import Continuation._

  def square(n: Int) = n * n

  // Continuation for square
  def squarec[R](n: Int) = unit[R](square(n))

  // Continuation for effect (Unit) on square.
  // This is simply to help the type inferencer by applying a type argument.
  def squareE(n: Int) = squarec[Unit](n)

  def main(args: Array[String]) {
    val k = squareE(args(0).toInt)
    k(println)
  }
}
~~~



A slightly more complicated example for modelling exceptions. Let's divide where the denominator must be 0 or an "exception" is thrown:


    
~~~{.Scala}
object Divide {
  import Continuation._

  // Division
  def div[R](c: String => Continuation[R, Int])
            (n: Int, // numerator
             d: Int) // denominator
                    : Continuation[R, Int] =
    callcc[R, Int, String](ok =>
      callcc[R, String, String](err =>
        if(d == 0) err("Denominator 0")
        else ok(n / d)
      ) flatMap c)

  def main(args: Array[String]) {
    def divError[R] = div[R](error(_)) _
    println(divError(7, 3)(x => x))
    println(divError(7, 0)(x => x)) // throws error
  }
}
~~~




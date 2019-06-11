---
comments: true
date: 2008-12-03 17:36:51
layout: post
slug: scalafunction1-lacking
title: scala.Function1 lacking
wordpressid: 471
tags: Programming
---

The Scala API leaves a lot to be desired. I'm going to pick on a few methods that should appear, but do not, on `[scala.Function1](http://www.scala-lang.org/docu/files/api/scala/Function1.html)`.

They are:




  * `map`


  * `flatMap`


  * `<*>` [(the S combinator)](http://en.wikipedia.org/wiki/SKI_combinator_calculus)


  * `on`



Using some magic with the `implicit` keyword I can make it appear as if these methods did in fact exist:


    
~~~{.Scala}
sealed trait RichFunction1[-T, +R] {
  def apply(t: T): R

  import RichFunction1.rich

  def map[X](g: R => X) = rich[T, X](g compose (apply(_)))

  def flatMap[TT <: T, X](g: R => RichFunction1[TT, X]) =
    rich[TT, X](t => g(apply(t))(t))

  // The S combinator (SKI)
  def <*>[TT <: T, X](f: RichFunction1[TT, R => X]) = (t: TT) => f(t)(apply(t))

  // S again, swapped arguments
  def <*>:[TT <: T, X](f: RichFunction1[TT, R => X]) = <*>(f)

  // map with swapped arguments
  def <-:[X](g: R => X) = map(g)

  def on[K](f: (R, R) => K, t1: T, t2: T): K = f(apply(t1), apply(t2))
}

object RichFunction1 {
  implicit def rich[T, R](f: T => R) = new RichFunction1[T, R] {
    def apply(t: T) = f(t)
  }
}
~~~



By having `flatMap` (and therefore `map`) this allows you to remove a lot of duplication. This may come at the expense of syntactical noise per Scala, but not always. Suppose you were given a String and you wanted to check if it was equal to one of a few Strings (ignoring case). You could use some trickery with existing methods on `List`, but I want to keep this example simple, so let us ignore that possibility for now (and accept that I could come up with a sufficient example that such trickery is insufficient).


    
~~~{.Scala}
// For example, suppose this predicate function
def predicate(s1: String) = s1 equalsIgnoreCase (_: String)
~~~



Here is the repetition


    
~~~{.Scala}
// predicate(s, _) repeats
def f(s: String) = predicate("x")(s) || predicate("y")(s) || predicate("z")(s)
~~~



But if we have `flatMap` and `map` we can use a for-comprehension:


    
~~~{.Scala}
// Taking advantage of flatMap/map
val g = for(a <- predicate("x");
            b <- predicate("y");
            c <- predicate("z"))
        yield a || b || c
~~~



Here is how that same code looks when expanded:


    
~~~{.Scala}
// Expansion of g
val h = predicate("x") flatMap (a =>
        predicate("y") flatMap (b =>
        predicate("z") map ((c =>
          a || b || c))))
~~~



How about some fancy stuff with the S combinator (`<*>`):


    
~~~{.Scala}
val or = Function.curried((_: Boolean) || (_: Boolean) || (_: Boolean))

// Using the S combinator
val i = predicate("z") <*>
        (predicate("y") <*>
        (predicate("x") map
        or))
~~~



Or with the arguments swapped around:


    
~~~{.Scala}
// Using S with swapped arguments
val j = ((or <-: predicate("x")) <*>: predicate("y")) <*>: predicate("z")
~~~



Pretty neat eh?

Suppose you wanted to check if the length of one List was less than the length of another. You might be tempted to write `x.length < y.length`. Notice how `_.length` repeats? Again I want to keep this example simple so while the solution below is more noisy, there are cases where it is not. 

Scala is let down a little by first-class function semantics. We'll begin with assuming this first-class function value:

    
~~~{.Scala}
val length = (_: List[Int]).length
~~~



Then comparing using length:


    
~~~{.Scala}
val k = length on (_ < _, List(4, 5, 6, 7), List(1, 2, 3))
~~~



A bit noisier but the repetition is gone. It's a shame that abstraction comes at a syntactic cost and in some cases it may even be worth that cost. I wish I had the choice.

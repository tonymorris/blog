---
comments: true
date: 2008-07-03 16:42:55
layout: post
slug: applicative-functor-laws-using-reductio-scala
title: Applicative Functor laws using Reductio (Scala)
wordpressid: 93
tags: Programming
---

Here is the `Applicative` functor type-class (_see [Applicative Programming with Effects](http://www.soi.city.ac.uk/~ross/papers/Applicative.html), Conor McBride, Ross Paterson_):


    
~~~{.Haskell}
class Applicative f where
  pure :: a -> f a
  (<*>) :: f (a -> b) -> f a -> f b
~~~



Here are the laws for the Applicative functor type-class:




  
  * identity  
`pure id <*> u == u`

  
  * composition  
`pure (.) <*> u <*> v <*> w == u <*> (v <*> w)`

  
  * homomorphism  
`pure f <*> pure x == pure (f x)`

  
  * interchange  
`u <*> pure x == pure (\f -> f x) <*> u`



Here are those laws stated using [Reductio](http://reductiotest.org/). I have changed `pure` to `unit` and the language is Scala:


    
    
~~~{.Scala}
object ApplicativeLaws {
  def identity[A[_], X](implicit a: Applicative[A],
                                 aax: Arbitrary[A[X]],
                                 e: Equal[A[X]]) =
    prop((apx: A[X]) => apx === a(a.unit((x: X) => x), apx))

  def composition[A[_], X, Y, Z](implicit a: Applicative[A],
                                aayz: Arbitrary[A[Y => Z]],
                                aaxy: Arbitrary[A[X => Y]],
                                aax: Arbitrary[A[X]],
                                e: Equal[A[Z]]) =
    prop((apyz: A[Y => Z], apxy: A[X => Y], apx: A[X]) =>
            a(apyz, a(apxy, apx)) ===
            a(a(a(a.unit((f: Y => Z) => (g: X => Y) => f compose g), apyz), apxy), apx))

  def homomorphism[A[_], X, Y](implicit a: Applicative[A],
                                        ax: Arbitrary[X],
                                        axy: Arbitrary[X => Y],
                                        e: Equal[A[Y]]) =
    prop((f: X => Y, x: X) => a(a.unit(f), a.unit(x)) === a.unit(f(x)))

  def interchange[A[_], X, Y](implicit a: Applicative[A],
                                       ax: Arbitrary[X],
                                       apxy: Arbitrary[A[X => Y]],
                                       e: Equal[A[Y]]) =
    prop((f: A[X => Y], x: X) =>
      a(f, a.unit(x)) === a(a.unit((f: X => Y) => f(x)), f))
}
~~~



Pretty neat eh? :) Let's test the `Applicative` instance for `List`:


    
~~~{.Scala}
List(identity[List, Int],
     composition[List, Int, Long, String],
     homomorphism[List, Int, String],
     interchange[List, Int, String]).
  foreach(p => summary println p)
~~~



This runs 100 unit tests per property, so 400 unit tests altogether. Here is the output:

    
    
    OK, passed 100 tests.
    OK, passed 100 tests.
    OK, passed 100 tests.
    OK, passed 100 tests.
    



Woot woot!

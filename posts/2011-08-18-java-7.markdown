---
comments: true
date: 2011-08-18 21:06:13
layout: post
slug: java-7
title: Java 7
wordpressid: 1030
tags: Programming
---

Java 7 has proposed syntax for what Scala calls two methods:



	
  1. 
            `Option.flatMap`{.Scala}
             In Scala where we would normally write:
            
    
~~~ {.Scala}
(a, b, c, d) => for {
  aa <- a(argsa)
  bb <- b(argsb)
  cc <- c(argsc)
  dd <- d(argsd)
} yield f(aa, bb, cc, dd)
~~~


While in Java we write:
            
    
~~~ {.Java}
// ceremony in the absence of closures
// has been omitted (and altered slightly) for brevity
a(argsa)?.b(argsb)?.c(argsc)?.d(argsd).f();
~~~


This syntax denotes the bind operation of `Option` monad, while Scala's for-comprehension works for _any_ monad. A pedantic point of note is that, from a particular perspective, Java's syntax is slightly weaker than monad syntax, in that it is in fact, an applicative functor comprehension (not monad), since subsequent computations do not have access to previously computed values along the chain (if this is confuzzling, never mind -- it's important point otherwise, but not so much here). Scala's for-comprehensions allow this, but it hasn't been demonstrated above.
        

	
  2. `Option.getOrElse`{.Scala}

Java calls this `?:`, so while in Scala we might write `value getOrElse k`, or if you prefer Scalaz, `value | k`, in Java we'd write `value ?: k`. The Java version maintains the usual lack of safety of `null`, while Scala uses an algebraic data type.


Among many differences between Scala and Java here, one is that Scala does not introduce syntax for these two specific functions -- after all, why would you? Further, what about the zillions of other useful functions? Java has no user-defined call-by-need unification, which means it is not possible to write `?:` yourself (even with a different valid Java identifier as a name). [I wrote about this once before](http://blog.tmorris.net/a-fling-with-lazy-evaluation/). I expect this is the reason for introduction of syntax for representing individual functions.

Scala's representation is also safer, in that it uses a data structure to denote _a list with a maximum length of one_, rather than _a value with type T, oh wait, just kidding, it might be null!_

My favourite part of Java's proposed introduction of syntax for two very specific functions (among hundreds of others), watered down to be not-quite-as-useful, is the assurance that I am still going to hear about how Scala is complex, while Java is not. Fun times.

_Edit: Proposed Java syntax._

---
comments: true
date: 2008-06-20 09:39:02
layout: post
slug: applicative-functors-in-scala
title: Applicative Functors in Scala
wordpressid: 87
tags: Programming
---

The [Applicative Functor](http://www.soi.city.ac.uk/~ross/papers/Applicative.html) pattern is an incredibly powerful abstraction. I recently added it to a branch of [Scalaz](http://wiki.workingmouse.com/index.php/Scalaz). However, it would be nice if I could alter the fixity of functions so that the parentheses below are not required to make the expression right-associative.


    
~~~{.Scala}
val add = (a: Int) => (b: Int) => (c: Int) => a + b + c
val none: Option[Int] = None // Grrr Scala

// Some(24)
println(Some(7) <*> (Some(8) <*> (Some(9) > add)))

// None
println(Some(7) <*> (none <*> (Some(9) > add)))

// List(87, 88, 89, 91, 92, 93)
println(List(1, 2, 3) <*> (List(77) <*> (List(9, 13) > add)))
~~~



Having to write that silly `none` value is annoying too. At the very least a none function would suffice:


    
~~~{.Scala}
def none[A]: Option[A] = None
~~~



That way. I could use `none[Int]` and be done with it.

Still, this pattern is a nice tool to use and I will surely be using it in the future.

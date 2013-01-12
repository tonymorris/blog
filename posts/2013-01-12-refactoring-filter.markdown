---
comments: true
date: 2013-01-12 19:00.00
layout: post
slug: refactoring-filter
title: Refactoring filter
tags: Programming
---

The `filter` function is one that accepts a list as an argument, keeping all elements that satisfy a given predicate and returns the resulting list. For example, I might use the `filter` function in Haskell to keep all **even** elements in a list:

~~~{.Haskell}
λ> filter even [2,3,6,2,4,7,6,75,22]
[2,6,2,4,6,22]
~~~

or perhaps using Scala:

~~~{.Scala}
scala> List(2,3,6,2,4,7,6,75,22)
res0: List[Int] = List(2, 3, 6, 2, 4, 7, 6, 75, 22)
~~~

In both cases, the list structure under examination is a [cons list](http://en.wikipedia.org/wiki/Cons).

When I teach functional programming, we are usually implementing the `filter` function on [our own cons list using Haskell](https://github.com/tonymorris/course/blob/master/src/L02/List.hs#L80)[^1]. Having done this enough times now, both with Haskell and Scala, I observe a recurring pattern.

The usual solutions are given either with explicit pattern-matching and recursion:

~~~{.Haskell}
fiilter _ [] = []
fiilter p (h:t) = if p h then h:fiilter p t else fiilter p t
~~~

…or by using the provided `foldRight` function, which implements the explicit pattern-matching and recursion on a cons list for the general case:

~~~{.Haskell}
fiilter p x = foldRight (\a b -> if p a then a:b else b) [] x
~~~

Whichever solution is given, they are both correct. However, almost invariably, at this stage in the exercise there is a developing suspicion that this solution can be *done better*. For example, in the pattern-matching solution, both sides of the `if` branch repeat the code `fiilter p t`. Indeed, even in the `foldRight` solution, there is application to the value `b` on both sides of the `if` branch.

[^1]: The function names have been altered to prevent clashing with those built-in.


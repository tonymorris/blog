---
comments: true
date: 2010-03-23 09:12:36
layout: post
slug: i-have-found-that
title: I have found that...
wordpressid: 703
tags: Programming
---

> 
I have found that many classes are created for the specific purpose of a `sequence` function in the `((->) t)` monad, particularly in Java, C# and Python. Create a class, with a constructor that takes an argument (t) so that one may call many of its methods, each of which has access to t.




    
~~~{.Haskell}
sequence :: [m a] -> m [a]
sequence* :: [t -> a] -> t -> [a]
~~~



If we consider the type of `t` which we shall call `Swizzle` and the created class we shall call `SwizzleManager` then


    
~~~{.Java}
// A constructor accepting a Swizzle
SwizzleManager m = new SwizzleManager(t);
// One of the following usually follow:
//   A list constructed by the results of executing several methods on 'm'
List<r> = { m.a(x), m.b(y), m.c(z) }
//   A loop over a list of the results of executing several methods on 'm'
for(R r : { m.a(x), m.b(y), m.c(z) }) {
  use(r);
}
//   A list of effects to execute using 'm'
m.a(x);
m.b(y);
m.c(z);
~~~

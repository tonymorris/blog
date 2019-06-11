---
comments: true
date: 2008-12-26 14:40:56
layout: post
slug: controlling-effects-with-flatmap
title: Controlling effects with flatMap/>>=
wordpressid: 494
tags: Programming
tag: Functional Programming, Haskell, Monads, Scala
---

I used to lecture at university. These days I teach for fun. Sometimes I am asked about controlling effects in a pure programming language such as Haskell by people who are very familiar with Scala's `flatMap` method on List type, Option and so on (sadly, it is missing from the most fundamental of all -- `Function1`). I often do this on a white-board and it has been reasonably successful at producing "aha! moments" so maybe it will help you.

Scala includes special syntax for using this method in the form of for-comprehensions. Specifically code like the following...

    
~~~{.Haskell}
for(k <- e1;
    l <- e2(k);
    m <- e3;
    n <- e4(k, m))
  yield f(k, l, m, n)
~~~


...is translated into...

    
~~~{.Scala}
e1 flatMap (k =>
e2(k) flatMap (l =>
e3 flatMap (m =>
e4(k, m) map (n =>
f(k, l, m, n)))))
~~~


Notice the final call to `map` which is a _specialisation_ of a call to `flatMap` by taking the _unital_ for the type constructor under consideration. Gobbledy-gook? It's simple really. Consider Lists:

    
~~~{.Scala}
x map f
// can also be written
x flatMap (z => List(f(z)))
~~~


That is because `x => List(x)` is the unit for `List`. For `Option` this function is the unit operation: `x => Some(x)`. Try it next time you call `map` -- use `flatMap` instead plus the unit operation for whatever type constructor you're using (`List`, `Option`, whatever) -- you'll always get the same result. For `Function1` the unit operation is `x => y => x`. I digress.

When I am asked about controlling side-effects I point to this `flatMap` business first so that I can assume familiarity (simple right?), then I switch to a completely different discussion about say, the following Java program snippet:

    
~~~{.Scala}
T t = e1();
e2(t);
U u = e3(t);
V v = e4(t, u);
return e5(u, v);
~~~



What I do with this snippet is invent a new programming that has very similar, but still different syntax to Java and I rewrite the program above. I do two things first




  1. The syntax for assignment occurs in reverse


  2. I remove type annotations (like T, U and V)



Here is how the program looks now:


    
~~~{.Scala}
e1() = t;
e2(t);
e3(t) = u;
e4(t, u) = v;
return e5(u, v);
~~~



Next I replace semi-colons (except the last one) with `=>` and I rename `return` to `unit`:


    
~~~{.Scala}
e1() = t =>
e2(t) =>
e3(t) = u =>
e4(t, u) = v =>
unit e5(u, v)
~~~



Finally, I replace the equals sign with `flatMap` and I add a special case for the call to `e2` which has no return value by calling `flatMap` but ignoring the parameter to the given function (denoted with an underscore):


    
~~~{.Scala}
e1() flatMap t =>
e2(t) flatMap _ =>
e3(t) flatMap u =>
e4(t, u) flatMap v =>
unit e5(u, v)
~~~



Since we know that the call to `flatMap` with `unit` can be replaced with a call to `map` let's do that:


    
~~~{.Scala}
e1() flatMap t =>
e2(t) flatMap _ =>
e3(t) flatMap u =>
e4(t, u) map v =>
e5(u, v)
~~~



Voila! Have we just turned an imperative program into a pure one just by altering syntax!? Not really -- rather, the distinction between imperative and pure is entirely dependent on the colour of your glasses. There is no hard-and-fast divide between one and the other (let that not detract from the huge implications of using one or the other). Importantly, we have seen that we can control side-effects with `flatMap`.

The `flatMap` method is at the very essence of a computational model called monads. Note that while we can control side-effects with monads, monads are not always about controlling side-effects! Indeed, when we call `flatMap` on `Option` or `List` we are using monads that have nothing to do with side-effects.

Haskell calls `flatMap` by a different name `>>=` and instead of for-comprehensions, Haskell has do-notation.

Pretty simple really isn't it? :)

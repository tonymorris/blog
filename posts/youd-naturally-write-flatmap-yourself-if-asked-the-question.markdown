---
comments: true
date: 2008-06-25 10:04:45
layout: post
slug: youd-naturally-write-flatmap-yourself-if-asked-the-question
title: You’d naturally write flatMap yourself if asked the question
wordpressid: 89
tags: Programming
---

Many people struggle to understand those fluffy things called _Monads_ and why they are important. I'm not going to attempt to alleviate this to a large degree, but I have had a recent success with a friend in having them attempt to write a familiar Scala function as a method. That is, the `[List.flatten](http://www.scala-lang.org/docu/files/api/scala/List$object.html#flatten(List[List[A]]))` function, which simply takes a `List` of `List` of some type `A` and returns a `List[A]`. It does this by concatenating all the lists together. I think most people are familiar with this function and have a good mental model of how it works. If this is the case and you are less confident about `List.flatMap`, then I hope to bring a point to your attention that might help you bring it home.

If you take a look at `List.sort`, you see it takes a function `(A, A) => Boolean`. This is because the type parameter to `List` is 'just any A'. It would be nice if it was 'any A, so long as it has defined order'. That way, you can just call `list.sort` and be done with it. This would require the creation of a new class with a more restricted type parameter; for example, you might pass the function at list creation time, like you do with a `TreeMap`.

Imagine writing `List.flatten` on the `List[A]` class as a _method_ using the same technique. You wouldn't be able to, since the method belongs on a `List[List[A]]`. You'd need to write the method such that it takes an argument: `A => List[A]` before you could then call `flatten`. When you have a `List[List[A]]`, you'd just call this method with the identity function `x => x` to obtain your resulting `List[A]`. Here is how the method would look:


    
~~~{.Scala}
def flatten(f: A => List[A]): List[A] = ...
~~~



Guess what?! This method is `flatMap`! Let's ignore the fact that the Scala API is significantly broken, including the `List.flatMap` method using `Iterable` in place of `List` here. Notice though, that `flatMap` is actually generalised by taking another type parameter, so we have just invented an unnecessarily specialised version of `flatMap`. Let's fix it:


    
~~~{.Scala}
def flatten[B](f: A => List[B]): List[B] = ...
~~~



Woot woot! In other words, `flatten(list)` is equivalent to `list flatMap (x => x)`. Furthermore, this should hold for more than just list, but for other type constructors too (sadly, `Option` is missing a flatten function: `Option[Option[A]] => Option[A]`). This is a special relationship that all monads have (join is a synonym for flatten and bind is a synonym for flatMap):



~~~{.Haskell}
join = bind id
~~~



We can express this using [Reductio](http://reductiotest.org/) (the Scala API of course!):


    
~~~{.Scala}
val prop_flat = prop((t: List[List[Int]]) => List.flatten(t) == t.flatMap(x => x))
// OK passed 100 tests.
~~~



I hope this helps. If it doesn't, ask a question or ignore my ranting :)

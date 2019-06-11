---
comments: true
date: 2008-09-22 12:57:36
layout: post
slug: partially-applying-scala-type-variables
title: Partially Applying Scala type variables
wordpressid: 337
tags: Programming
---

Below is a neat trick that others may find useful when designing APIs using the Scala programming language.

Scala's type inferencer is not as clever as some. As a result, we often find ourselves explicitly annotating type variables in certain contexts. This can become a little annoying, but what is worse, is if you have a type variable list and only one of those requires explicit specification, the others must also be explicitly specified even though they would have otherwise been inferred.

Let me demonstrate. I will use Scala's higher-kinds, which are never inferred (this is quite acceptable given the problems associated with inferring higher-kinds -- consider Java and C# where they do not even exist, thus leading to an enormous repetitive chore) though other examples may be applicable. However, it can be annoying if you were to write say, the Monad join function:

    
~~~{.Scala}
def join[M[_], A](m: M[M[A]]): M[A] = error("todo")
~~~


A caller invocation might look like:

    
~~~{.Scala}
val x = join[List, Int](List(List(1, 2, 3), List(4, 5, 6)))
~~~


Notice the explicit annotation with the `List` type constructor, however, the second type variable (`Int`) must also be explicitly specified, even though it could have otherwise been inferred. This is a bit annoying.

We can do away with it like so:

    
~~~{.Scala}
def join[M[_]] = new {
  def apply[A](m: M[M[A]]): M[A] = error("todo")
}
~~~


And now the caller code no longer has that clumsy type annotation:

    
~~~{.Scala}
val x = join[List](List(List(1, 2, 3), List(4, 5, 6)))
~~~



Yay! Have a nice day :)

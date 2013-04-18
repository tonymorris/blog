---
comments: true
date: 2008-01-16 15:06:39
layout: post
slug: scalaoption-cheat-sheet
title: scala.Option Cheat Sheet
wordpressid: 57
tags: Programming
---

Many people who are coming in to Scala first encounter the [Option](http://www.scala-lang.org/docu/files/api/scala/Option.html) type, which may be thought of (among other things) as a type-safe `null`. They also encounter pattern matching as both a new and (relatively) powerful concept, but also one that is easy to understand. This leads to quite a lot of use of pattern matching and often excessively so in what I have observed.

Particularly with a type as trivial as `Option`, it is almost always possible to do away with pattern matching by using a higher-order function. Use of this function is typically preferred over pattern matching as tighter code. In fact, it is important to observe that it is possible to encapsulate all forms of pattern matching over `Option` with one simple higher-order function:

    
~~~{.Scala}
def option[A, X](o: Option[A])(none: => X, some: => A => X): X = ...
~~~

Then, all functions can be written in terms of this one and needn't pattern match at all. For example, consider `Option.map`:

~~~{.Scala}
def map[A, B](o: Option[A], f: A => B) =
  option(o, None, a => Some(f(a)))
~~~

In this post, I am going to give some common uses of pattern matching, which many developers might find themselves performing, followed by the use of a function that already exists on `Option` that encapsulates that given form of pattern matching. If you find yourself using pattern matching in a form not listed below, but feel it could be abstracted, then chances are that such a function exists in the [Scalaz extension to scala.Option](http://code.google.com/p/scalaz/).

I will use the identifier `foo` below to denote any particular function, including many functions composed, for example, `foo(x)` may represent the composition of two functions f and g: `f(g(x))`. I also use the identifier `option` to denote any value of the type `scala.Option`.

I hope this helps :)


----

**`flatMap`**

~~~{.Scala}
option match {
  case None => None
  case Some(x) => foo(x)
}
~~~

This code is equivalent to:

~~~{.Scala}
option.flatMap(foo(_))
~~~


----

**`flatten`**

~~~{.Scala}
option match {
  case None => None
  case Some(x) => x
}
~~~

This code is equivalent to:

~~~{.Scala}
option.flatten
~~~


----

**`map`**

~~~{.Scala}
option match {
  case None => None
  case Some(x) => Some(foo(x))
}
~~~

This code is equivalent to:

~~~{.Scala}
option.map(foo(_))
~~~


----

**`foreach`**

~~~{.Scala}
option match {
  case None => {}
  case Some(x) => foo(x)
}
~~~

This code is equivalent to:

~~~{.Scala}
option.foreach(foo(_))
~~~


----

**`isDefined`**

~~~{.Scala}
option match {
  case None => false
  case Some(_) => true
}
~~~

This code is equivalent to:

~~~{.Scala}
option.isDefined
~~~


----

**`isEmpty`**

~~~{.Scala}
option match {
  case None => true
  case Some(_) => false
}
~~~

This code is equivalent to:

~~~{.Scala}
option.isEmpty
~~~


----

**`forall`**

~~~{.Scala}
option match {
  case None => true
  case Some(x) => foo(x)
}
~~~

This code is equivalent to:

~~~{.Scala}
option.forall(foo(_))
~~~


----

**`exists`**

~~~{.Scala}
option match {
  case None => false
  case Some(x) => foo(x)
}
~~~

This code is equivalent to:

~~~{.Scala}
option.exists(foo(_))
~~~


----

**`orElse`**

~~~{.Scala}
option match {
  case None => foo
  case Some(x) => Some(x)
}
~~~

This code is equivalent to:

~~~{.Scala}
option.OrElse(foo)
~~~


----

**`getOrElse`**

~~~{.Scala}
option match {
  case None => foo
  case Some(x) => x
}
~~~

This code is equivalent to:

~~~{.Scala}
option.getOrElse(foo)
~~~


----

**`toList`**

~~~{.Scala}
option match {
  case None => Nil
  case Some(x) => x :: Nil
}
~~~

This code is equivalent to:

~~~{.Scala}
option.toList
~~~


----


**`coflatMap`**[^1]

~~~{.Scala}
option match {
  case None => None
  case Some(_) => Some(foo(option))
}
~~~

This code is equivalent to:

~~~{.Scala}
option.coflatMap(foo(_))
~~~


----

**`duplicate`**[^2]

~~~{.Scala}
option match {
  case None => None
  case Some(_) => Some(option)
}
~~~

This code is equivalent to:

~~~{.Scala}
option.duplicate
~~~

[^1]: Unfortunately `coflatMap` is not part of the standard library. You will need to write it yourself or use Scalaz.

[^2]: Unfortunately, `duplicate` is not part of the standard library. You will need to write it yourself or use Scalaz.

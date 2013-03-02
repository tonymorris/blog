---
comments: true
date: 2013-03-01 12:00.00
layout: post
slug: functors-and-things-using-scala
title: Functors and things using Scala
tags: Programming, Scala, Functor, Monad, Comonad, Applicative
---

## Types of Functors

There are many types of functors. They can be expressed using the Scala programming language.

* covariant functors — defines the operation commonly known as `map` or `fmap`.

~~~{.Scala}
// covariant functor
trait Functor[F[_]] {
  def fmap[A, B](f: A => B): F[A] => F[B]
}
~~~

* contravariant functors — defines the operation commonly known as `contramap`.

~~~{.Scala}
// contravariant functor
trait Contravariant[F[_]] {
  def contramap[A, B](f: B => A): F[A] => F[B]
}
~~~

* exponential functors — defines the operation commonly known as `xmap`. Also known as *invariant functors*.

~~~{.Scala}
// exponential functor
trait Exponential[F[_]] {
  def xmap[A, B](f: (A => B, B => A)): F[A] => F[B]
}
~~~

* applicative functor[^1] — defines the operation commonly known as `apply` or `<*>`.

~~~{.Scala}
// applicative functor (abbreviated)
trait Applicative[F[_]] {
  def apply[A, B](f: F[A => B]): F[A] => F[B]
}
~~~

* monad[^2] — defines the operation commonly known as `bind`, `flatMap` or `=<<`.

~~~{.Scala}
// monad (abbreviated)
trait Monad[F[_]] {
  def flatMap[A, B](f: A => F[B]): F[A] => F[B]
}
~~~

* comonad[^3] — defines the operation commonly known as `extend`, `coflatMap` or `<<=`.

~~~{.Scala}
// comonad (abbreviated)
trait Comonad[F[_]] {
  def coflatMap[A, B](f: F[A] => B): F[A] => F[B]
}
~~~

## Remembering the different types

Sometimes I am asked how to remember all of these and/or determine which is appropriate. There are many answers to this question, but there is a common feature of all of these different types of functor:

> They all take an argument that is some arrangement of three type variables
> and then return a function with the type F[A] => F[B].

I memorise the table that is the type of the different argument arrangements to help me to determine which abstraction might be appropriate. Of course, I use other methods, but this particular technique is elegant and short. Here is that table:

------------------------------------
functor         argument arrangement
--------------- --------------------
covariant       `A => B`{.Scala}

contravariant   `B => A`{.Scala}

exponential     `(A => B, B => A)`{.Scala}

applicative     `F[A => B]`{.Scala}

monad           `A => F[B]`{.Scala}

comonad         `F[A] => B`{.Scala}
------------------------------------

We can see from this table that there is not much reason to emphasise one over the other. For example, monads get *lots* of attention and associated stigma, but it's undeserved. It's rather boring when put in the context of a bigger picture. It's just a different arrangement of its argument (`A => F[B]`).

Anyway, this table is a good way to keep a check on the different types of abstraction and how they might apply. There are also ways of deriving some from others, but that's for another rainy day. That's all, hope it helps!

[^1]: applicative functors also define an identity operation (`def insert[A]: A => F[A]`) however, it is omitted.
[^2]: monads also define an identity operation (`def insert[A]: A => F[A]`) however, it is omitted.
[^3]: comonads also define an identity operation (`def extract[A]: F[A] => A`) however, it is omitted.
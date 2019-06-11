---
comments: true
date: 2008-11-30 18:58:51
layout: post
slug: observefunctormonad
title: ObserveFunctorMonad
wordpressid: 457
tags: Programming
---

An exercise arising from an IRC discussion in #scala:


    
~~~{.Scala}
    trait Functor[F[_]] {
      def fmap[A, B](fa: F[A], f: A => B): F[B]
    }
    
    trait Monad[M[_]] {
      def bind[A, B](ma: M[A], f: A => M[B]): M[B]
      def pure[A](a: A): M[A]
    }
    
    object ObserveFunctorMonad {
      def observe[K[_]](m: Monad[K]): Functor[K] = error("your homework")
    }
~~~

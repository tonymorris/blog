---
comments: true
date: 2013-06-22 20:00:00
layout: post
slug: scala-type-class-hierarchy
title: Scala Type-class Hierarchy
tags: Programming, Scala
---

Below is a proposed type-class hierarchy demonstrated using the Scala programming language. The goal of the hierarchy is to demonstrate a hierarchy that is useful and makes the appropriate trade-offs for general-purpose programming. It is therefore, independent of programming language, however some (many) programming-language type systems do not have the expressive power to witness the hierarchy.

This proposal is in the spirit of [the typeclassopedia](http://www.haskell.org/haskellwiki/Typeclassopedia) with the following differences:

1. Use the Scala programming language for demonstration for those who prefer it.

2. Discussion need not concern itself with any kind of backward compatibility.

You will find a similar type-class hierarchy in [the Scalaz library](http://github.com/scalaz/scalaz). That implementation is far more comprehensive and is aimed primarily for production use. A secondary goal here is to help document the Scalaz hierarchy in terse form, however, note that you will find some minor differences.

Discussion about addition or rearrangement of the proposed hierarchy is welcome.

~~~{.Scala}
trait ~>[F[_], G[_]] {
  def apply[A]: F[A] => G[A]
}

case class Id[A](x: A)

trait Semigroup[M] {
  def op: M => M => M
}

trait Monoid[M] extends Semigroup[M] {
  val id: M
}

trait Functor[F[_]] {
  def fmap[A, B](f: A => B): F[A] => F[B]
}

trait Apply[F[_]] extends Functor[F] {
  def ap[A, B](f: F[A => B]): F[A] => F[B]
}

trait Bind[F[_]] extends Apply[F] {
  def bind[A, B](f: A => F[B]): F[A] => F[B]
}

trait Applicative[F[_]] extends Apply[F] {
  def insert[A]: A => F[A]
}

trait Monad[F[_]] extends Applicative[F] with Bind[F]

trait Extend[F[_]] extends Functor[F] {
  def extend[A, B](f: F[A] => B): F[A] => F[B]
}

trait Comonad[F[_]] extends Extend[F] {
  def extract[A]: F[A] => A
}

trait Contravariant[F[_]] {
  def contramap[A, B](f: B => A): F[A] => F[B]
}

trait Distributive[T[_]] extends Functor[T] {
  def distribute[F[_]: Functor, A, B](f: A => T[B]): F[A] => T[F[B]]
}

trait Foldable[T[_]] {
  def foldMap[A, M: Monoid](f: A => M): T[A] => M
}

trait Foldable1[T[_]] extends Foldable[T] {
  def foldMap1[A, M: Semigroup](f: A => M): T[A] => M
}

trait Traversable[T[_]] extends Functor[T] with Foldable[T] {
  def traverse[F[_]: Applicative, A, B](f: A => F[B]): T[A] => F[T[B]]
}

trait Traversable1[T[_]] extends Traversable[T] with Foldable1[T] {
  def traverse1[F[_]: Apply, A, B](f: A => F[B]): T[A] => F[T[B]]
}

trait MonadTransformer[T[_[_], _]] {
  def lift[M[_]: Monad, A]: M[A] => T[M, A]
}

trait BindTransformer[T[_[_], _]] extends MonadTransformer[T] {
  def liftB[M[_]: Bind, A]: M[A] => T[M, A]
}

trait MonadTransform[T[_[_], _]] {
  def transform[F[_]: Monad, G[_]: Monad, A](f: F ~> G): T[F, A] => T[G, A]
}

trait BindTransform[T[_[_], _]] extends MonadTransform[T] {
  def transformB[F[_]: Bind, G[_]: Monad, A](f: F ~> G): T[F, A] => T[G, A]
}

trait ComonadTrans[T[_[_], _]] {
  def lower[M[_]: Comonad, A]: T[M, A] => M[A]
}

trait ExtendTrans[T[_[_], _]] {
  def lowerE[M[_]: Extend, A]: T[M, A] => M[A]
}

trait ComonadHoist[T[_[_], _]] {
  def cohoist[M[_]: Comonad, A]: T[M, A] => T[Id, A]
}

trait ExtendHoist[T[_[_], _]] {
  def cohoistE[M[_]: Extend, A]: T[M, A] => T[Id, A]
}

trait Semigroupoid[~>[_, _]] {
  def compose[A, B, C]: (B ~> C) => (A ~> B) => (A ~> C)
}

trait Category[~>[_, _]] extends Semigroupoid[~>] {
  def id[A]: A ~> A
}

trait First[~>[_, _]] extends Semigroupoid[~>] {
  def first[A, B, C]: (A ~> B) => ((A, C) ~> (B, C))
}

trait Arrow[~>[_, _]] extends Category[~>] with First[~>] {
  def idA[A, B]: (A => B) => (A ~> B)
}
~~~

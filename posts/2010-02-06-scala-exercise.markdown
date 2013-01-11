---
comments: true
date: 2010-02-06 09:37:10
layout: post
slug: scala-exercise
title: Scala exercise
wordpressid: 649
---

A result of a discussion in the [#scala IRC channel](irc://freenode.net/#scala).

**Write a minimum function that works on `Array[String]` and `List[Int]`.** _(see `error("todo")`)_


    
~~~{.Scala}
trait Foldable[-F[_]] {
  def foldl[A, B](f: (A, B) => A, a: A, as: F[B]): A

  def reducel[A](f: (A, A) => A, as: F[A]): Option[A] = foldl[Option[A], A]((a1, a2) =>
    Some(a1 match {
      case None => a2
      case Some(x) => f(a2, x)
    }), None, as)
}

object Foldable {
  val ListFoldable = new Foldable[List] {
    def foldl[A, B](f: (A, B) => A, a: A, as: List[B]) =
      as.foldLeft(a)(f)
  }

  val ArrayFoldable = new Foldable[Array] {
    def foldl[A, B](f: (A, B) => A, a: A, as: Array[B]) =
      as.foldLeft(a)(f)
  }
}

sealed trait Ordering
case object LT extends Ordering
case object EQ extends Ordering
case object GT extends Ordering

// contra
trait Order[A] {
  def compare(a1: A, a2: A): Ordering

  def min(a1: A, a2: A) =
    if(compare(a1, a2) == LT) a1 else a2
}

object Order {
  def order[A](f: (A, A) => Ordering): Order[A] = new Order[A] {
    def compare(a1: A, a2: A) = f(a1, a2)
  }

  val IntOrder = order[Int]((a1, a2) =>
    if(a1 > a2) GT
    else if(a1 < a2) LT
    else EQ)

  val StringOrder = order[String]((a1, a2) =>
    if(a1 > a2) GT
    else if(a1 < a2) LT
    else EQ)
}

object Main {
  import Foldable._
  import Order._

  def minimum[F[_], A](as: F[A], order: Order[A], fold: Foldable[F]) =
    // Zm9sZC5yZWR1Y2VsW0FdKChhLCBiKSA9PiBvcmRlci5taW4oYSwgYiksIGFzKQ==
    error("todo")

  def main(args: Array[String]) {
    val i = minimum(args, StringOrder, ArrayFoldable)
    println(i)

    val j = minimum(List(5, 8, 2, 9), IntOrder, ListFoldable)
    println(j)
  }
}
~~~

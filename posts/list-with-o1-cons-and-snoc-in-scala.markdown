---
comments: true
date: 2011-02-20 10:40:48
layout: post
slug: list-with-o1-cons-and-snoc-in-scala
title: List with O(1) cons and snoc in Scala
wordpressid: 933
tags: Programming
---

A pseudo difference list with constant time prepend (cons) and append (snoc).

[There is an efficiency issue](http://code.google.com/p/scalaz/issues/detail?id=19) with respect to Scala's TCO implementation (making this data structure untenable?), however, I have forgotten the details of that.

In any case, a finger-tree is often more appropriate, but it would be nice to revive the `Endo[List[A]]`!


    
~~~{.Scala}
sealed trait DiffList[A] {
  val endo: List[A] => List[A]

  import DiffList._

  // cons O(1)
  def <::(a: A): DiffList[A] = new DiffList[A] {
    val endo = (z: List[A]) => a :: DiffList.this.endo(z)
  }

  // snoc O(1)
  def ::>(a: A): DiffList[A] = new DiffList[A] {
    val endo = (z: List[A]) => DiffList.this.endo(a :: z)
  }

  // append O(1)
  def :::>(a: DiffList[A]): DiffList[A] = new DiffList[A] {
    val endo = (z: List[A]) => DiffList.this.endo(a.endo(z))
  }

  // O(n)
  def toList = endo(Nil)
}

object DiffList {
  def empty[A]: DiffList[A] = new DiffList[A] {
    val endo = (z: List[A]) => z
  }

  def single[A](a: A): DiffList[A] = a <:: empty[A]
}
~~~

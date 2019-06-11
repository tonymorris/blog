---
comments: true
date: 2008-08-22 11:00:52
layout: post
slug: flippin-scala
title: Flippin' Scala
wordpressid: 252
tags: Programming
---

Once upon a time, I tried to write some Scala 2.7.1 for [Functional Java](http://functionaljava.org/) and [Reductio](http://reductiotest.org/), but the compiler broke. So I filed [the bug](https://lampsvn.epfl.ch/trac/scala/ticket/765) and waited for a fix. Thanks to Odersky et. al., the fix came promptly, so I started using one of the Scala 2.7.1->2.7.2 nightly builds and then I could compile my Scala code, yay! But that didn't stop [users complaining about crashes](https://issues.workingmouse.com/view.php?id=49) when using Scala 2.7.1 due its bad class file parser. Some users even [stopped using Reductio because they don't trust non-release builds](http://groups.google.com/group/reductio/browse_thread/thread/289340b14deecdaf). Oh well.

So I try out this new code snippet, nothing complicated:

    
~~~{.Scala}
class Foo[F[_]]

object Foo {
  def foo(n: Int)(implicit f: Foo[IN] forSome { type IN[_] }) = "foo"

  implicit val z: Foo[List] = new Foo[List]

  val t = foo(7)
}
~~~


But Scala 2.7.1 falls over. So does the 2.7.1->2.7.2 nightly build that I was using. Ugh! So I try out Scala 2.7.2-RC1, yay it compiles my legitimate source file. But does it compile all my other source?

Er no, [Scala 2.7.2-RC1 produces bad class files](http://lampsvn.epfl.ch/trac/scala/ticket/1270) when compiling code that is otherwise fine with previous Scala versions. In an attempt to obtain one bug fix I have also had to adopt another -- one that makes its use non-viable. I am stuck with dealing with the former bug, which is also somewhat non-viable but only slightly more so than the latter. Give me a friggin' break!

Ugh, this is not the first occurrence of this silliness. Flippin' Scala, does this shit ever stop?

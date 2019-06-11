---
comments: true
date: 2008-11-17 16:14:55
layout: post
slug: clean-up-resource-with-scala
title: Clean-up resource with Scala
wordpressid: 423
tags: Programming
---

> 
`Nov 17 15:48:39 <jinok>` i hate wasting vertical space declaring vals that i need to initialize outside of a try block so i can clean them up in a finally block
`Nov 17 15:48:53 <jinok>` so i've started doing the initialization using lazy vals.





    
~~~{.Scala}
sealed trait Resource[+T] {
  val value: T
  def close: Unit

  def use[X](f: T => X) = try { f(value) } finally { close }
}

import java.io.InputStream

object Resource {
  def resource[T](t: T, c: => Unit) = new Resource[T] {
    val value = t
    def close = c
  }

  implicit def InputStreamResource(t: InputStream) = resource(t, t.close)
}

object Example {
  import Resource._

  def main(args: Array[String]) {
    val in = new java.io.FileInputStream("/etc/passwd")
    // print the first character of /etc/passwd
    println(in use (_.read.toChar))
  }
}
~~~



Reader's exercise: `Resource` is a functor and monad. Implement the `map` and `flatMap` methods. Observe trickiness and ask why?

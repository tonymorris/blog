---
comments: true
date: 2008-10-01 09:08:01
layout: post
slug: scala-gotchya
title: 'Scala: Gotchya!'
wordpressid: 364
tags: Programming
---

I have been caught out by type-checking code that fails at runtime. It should have failed at compile-time due to a language limitation in structural types -- not a logical absurdity in the code itself. Thankfully I have a workaround, but it does provoke persistent hesitance -- giving that disgusting feeling as if I were using something as degenerate as, for example, Ruby or even worse (can it get worse?), Groovy. I'll have to counsel myself on that one.

Nevertheless, I give warning. Here is the [bug report](http://lampsvn.epfl.ch/trac/scala/ticket/1388). Here is the fun. Observe:


    
    
    scala> def left[B] = new { def apply[A](a: A): Either[A, B] = Left(a) }
    left: [B]java.lang.Object{def apply[A](A): Either[A,B]}
    
    scala> left[String](7)
    res6: Either[Int,String] = Left(7)
    
    scala> def left[B] = new { def apply[A](a: A, b: B): Either[A, B] = Left(a) }
    left: [B]java.lang.Object{def apply[A](A,B): Either[A,B]}
    
    scala> left(7, "")
    res8: Either[Int,java.lang.String] = Left(7)
    
    scala> left[String](7, "")
    java.lang.NoSuchMethodException: $anon$1.apply(java.lang.Object, java.lang.String)
            at java.lang.Class.getMethod(Class.java:1605)
            at .reflMethod$Method1(<console>:6)
            at .<init>(<console>:6)
            at .<clinit>(<console>)
            at RequestResult$.<init>(<console>:3)
            at RequestResult$.<clinit>(<console>)
            at RequestResult$result(<console>)
            at sun.reflect.NativeMethodAccessorImpl.invoke0(Native ...
    

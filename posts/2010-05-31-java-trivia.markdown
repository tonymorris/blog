---
comments: true
date: 2010-05-31 20:19:54
layout: post
slug: java-trivia
title: Java Trivia
wordpressid: 771
tags: Programming
---

Implement the missing function body. These rules must be followed:



	
  * No using `null`

	
  * No throwing an exception/error

	
  * Function must terminate

	
  * No side-effecting

	
  * No type-casing (`instanceof`)

	
  * No type-casting


How many different ways of achieving the objective?

~~~{.Java}
  interface Function<A, B> {
    B apply(A a);
  }

  class C {
    static <A, B, C> Function<A, C> c(final Function<A, B> f,
                                      final Function<B, C> g) {
        .... todo
    }
  }
~~~

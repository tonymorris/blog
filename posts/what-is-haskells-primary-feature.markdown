---
comments: true
date: 2010-01-22 19:30:37
layout: post
slug: what-is-haskells-primary-feature
title: What is Haskell's primary feature?
wordpressid: 636
tags: Programming
---

Today I was asked what Haskell's main feature is. The answer is its non-strict evaluation.

Java is a strictly evaluated language. Consider this Java program:

    
~~~{.Java}
class C {
  static String i() {
    throw new Error("boo!");
  }

  static <a> int f(A a) {
    return 3;
  }

  public static void main(String[] args) {
    int k = f(i());
    System.out.println(k);
  }
}
~~~



The program fails with a runtime error.

Consider this (otherwise equivalent) Haskell program:

    
~~~{.Haskell}
i = error "boo!"

f a = 3

main = let k = f i
       in print k
~~~



The program prints 3 and does not fail like the Java program. This is a key property of Haskell with very far reaching implications. One of those implications is that Haskell is a pure language. There are many more implications, particularly with respect to the compositional properties of programs. 

Haskell's evaluation model and its implications is perhaps its most widely misunderstood feature. While the benefits are (enormously) enormous, they are far too deep to consider writing a short article about.

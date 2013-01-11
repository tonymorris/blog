---
comments: true
date: 2008-10-03 14:04:43
layout: post
slug: project-euler-problem-2-functional-java
title: Project Euler Problem 2 Functional Java
wordpressid: 369
tags: Programming, Project Euler
---

[Project Euler Problem 2](http://projecteuler.net/index.php?section=problems&id=2) using [Functional Java](http://functionaljava.org/):


    
~~~{.Java}
import fj.data.Stream;
import fj.P1;
import fj.F2;
import static fj.function.Integers.even;
import static fj.pre.Ord.intOrd;
import static fj.pre.Monoid.intAdditionMonoid;
import static fj.data.Stream.cons;
import static fj.Function.curry;

...

Stream<integer> fibs = new F2<Integer, Integer, Stream<integer>>() {
  public Stream<integer> f(final Integer a, final Integer b) {
    return cons(a, P1.curry(curry(this).f(b)).f(a + b));
  }
}.f(1, 2);

final int problem2 = intAdditionMonoid.sumLeft(fibs.takeWhile(intOrd.isLessThan(1000001)).filter(even).toList());
~~~





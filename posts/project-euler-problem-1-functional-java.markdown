---
comments: true
date: 2008-10-03 13:59:39
layout: post
slug: project-euler-problem-1-functional-java
title: Project Euler Problem 1 Functional Java
wordpressid: 367
tags: Programming, Project Euler
---

[Project Euler Problem 1](http://projecteuler.net/index.php?section=problems&id=1) using [Functional Java](http://functionaljava.org/):


    
~~~{.Java}
import static fj.pre.Monoid.intAdditionMonoid;
import static fj.data.List.range;
import fj.F;

...

final int problem1 = intAdditionMonoid.sumLeft(range(0, 1000).filter(new F<Integer, Boolean>() {
  public Boolean f(final Integer a) {
    return a % 3 == 0 || a % 5 == 0;
  }
}));
~~~

---
comments: true
date: 2010-02-08 17:17:57
layout: post
slug: ski-combinator-calculus-in-java
title: SKI combinator calculus in Java
wordpressid: 651
tags: Programming
---

~~~{.Java}
interface Lam<X, Y> {
  Y apply(X x);
}

// http://en.wikipedia.org/wiki/SKI_combinator_calculus
class SKI {
  public static <A, B, C> Lam<Lam<A, Lam<B, C>>, Lam<Lam<A, B>, Lam<A, C>>> s() {
    return new Lam<Lam<A, Lam<B, C>>, Lam<Lam<A, B>, Lam<A, C>>>() {
      public Lam<Lam<A, B>, Lam<A, C>> apply(final Lam<A, Lam<B, C>> f) {
        return new Lam<Lam<A, B>, Lam<A, C>>() {
          public Lam<A, C> apply(final Lam<A, B> g) {
            return new Lam<A, C>() {
              public C apply(final A a) {
                return f.apply(a).apply(g.apply(a));
              }
            };
          }
        };
      }
    };
  }

  public static <A, B> Lam<A, Lam<B, A>> k() {
    return new Lam<A, Lam<B, A>>() {
      public Lam<B, A> apply(final A a) {
        return new Lam<B, A>() {
          public A apply(final B b) {
            return a;
          }
        };
      }
    };
  }

  public static <a> Lam<A, A> i() {
    return SKI.<A, Lam<A, A>, A>s().apply(SKI.<A, Lam<A, A>>k()).apply(SKI.<A, A>k());
  }
}
~~~

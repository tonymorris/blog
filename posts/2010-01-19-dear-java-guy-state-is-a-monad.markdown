---
comments: true
date: 2010-01-19 16:21:12
layout: post
slug: dear-java-guy-state-is-a-monad
title: Dear Java guy, State is a monad
wordpressid: 634
tags: Programming
---

~~~{.Java}
interface Pair<A, B> {
  A first();
  B second();
}

class Pairs {
  public static <A, B> Pair<A, B> pair(final A a, final B b) {
    return new Pair<A, B>() {
      public A first() { return a; }
      public B second() { return b; }
    };
  }
}

interface State<S, A> {
  Pair<S, A> run(S s);
}

interface Function<X, Y> {
  Y apply(X x);
}

class States {
  // State<S, _> is a covariant functor
  public static <S, A, B> State<S, B> map(final State<S, A> s, final Function<A, B> f) {
    return new State<S, B>() {
      public Pair<S, B> run(final S k) {
        final Pair<S, A> p = s.run(k);
        return Pairs.pair(p.first(), f.apply(p.second()));
      }
    };
  }

  // // State<S, _> is a monad
  public static <S, A, B> State<S, B> bind(final State<S, A> s, final Function<A, State<S, B>> f) {
    return new State<S, B>() {
      public Pair<S, B> run(final S k) {
        final Pair<S, A> p = s.run(k);
        return f.apply(p.second()).run(p.first());
      }
    };
  }
}
~~~

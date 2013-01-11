---
comments: true
date: 2010-01-28 16:48:14
layout: post
slug: higher-order-polymorphism-for-pseudo-java
title: Higher-order Polymorphism for pseudo-Java
wordpressid: 642
tags: Programming
---

~~~{.Java}
// Simulate Higher-Order Functions.
// A lambda function is any implementation of this interface.
interface Lambda<X, Y> {
  Y apply(X x);
}

// What is a covariant functor?
// It is any implementation of this interface.
// All implementations must also satisfy:
//   * The law of identity
//   * The law of composition
// (but let's brush that to the side)
interface Functor<f> {
  <A, B> F<b> fmap(F<a> a, Lambda<A, B> f);
}

import java.util.LinkedList;

// Here is the functor for Java's LinkedList.
// It maps a function across each element of the list and returns a new one.
// (trust me, it satisfies the two laws).
class LinkedListFunctor implements Functor<linkedlist> {
  public <A, B> LinkedList<b> fmap(final LinkedList<a> a, final Lambda<A, B> f) {
    final LinkedList<b> r = new LinkedList<b>();
    for(final A x : a)
      r.append(f.apply(x));
    return r;
  }
}

// Here is a trivial Java data type.
// It happens to be a covariant functor.
// Let's witness its instance...
interface IntConverter<a> {
  A convert(int i);
}

// The Functor instance for the IntConverter.
class IntConverterFunctor implements Functor<intconverter> {
  public <A, B> IntConverter<b> fmap(final IntConverter<a> a, final Lambda<A, B> f) {
    return new IntConverter<b>() {
      public B convert(final int i) {
        return f.apply(a.convert(i));
      }
    };
  }
}

// So why do we care?
// Because it prevents an enormous (*enormous*, *ENORMOUS!*)
// amount of otherwise needless repetition.

class FunctorX {
  // Gives rise to:
  // LinkedList<Lambda<A, B>> => A => LinkedList<b>
  // IntConverter<Lambda<A, B>> => A => IntConverter<b>
  // ...
  // F<Lambda<A, B>> => A => F<b> (for many values of F)
  public static <A, B, F> F<b> fapply(final Functor<f> f, final F<Lambda<A, B>> lam, final A a) {
    return f.fmap(lam, new Lambda<Lambda<A, B>, B>() {
      public B apply(final Lambda<A, B> z) {
        return z.apply(a);
      }
    });
  }

  // ... etcetra.
}

// It turns a linear amount of code into a constant amount of code.
// Similarly a sort function that runs on lists of
// any type (provided a comparator) alleviates the need for linear
// amounts of code (a sort function for each possible list element type).
~~~

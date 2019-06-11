---
comments: true
date: 2010-05-12 11:27:14
layout: post
slug: beginner-java-exercise-with-data-types
title: Beginner Java Exercise with Data Types
wordpressid: 760
tags: Programming
---

Below is a data type that represents a list that has a maximum length of one. It is often useful in cases where `null` would otherwise be used.

Consider for example, [the `getHeaders` method](http://java.sun.com/products/servlet/2.2/javadoc/javax/servlet/http/HttpServletRequest.html#getHeader%28java.lang.String%29) on `HttpServletRequest` which returns a `String` but might return `null` if there is no such header. Instead, a new API might return `NoneOrOne<String>` and do away with the use of `null`.

The idea of this exercise is to fill out the method bodies (remove the thrown `Error`) according to the comments without altering the method type. It is not permitted to use `null` or throw any exception. The tests at the bottom (see `main` method) should produce the specified results. At the moment, the code compiles, but will not execute successfully.

There are nine methods that need filling out. The tests are not exhaustive (use some intuition). Have fun! Questions are welcome.


    
~~~{.Java}
// A list that is either empty or has one element.
public abstract class NoneOrOne<a> {
  // The key abstract method (catamorphism).
  public abstract <x> X fold(Thunk<x> none, Func<A, X> one);

  // Produces an empty list.
  public static <a> NoneOrOne<a> none() {
    throw new Error("todo");
  }

  // Produces a list with the given element.
  public static <a> NoneOrOne<a> one(final A a) {
    throw new Error("todo");
  }

  // Returns true if this list is empty.
  public boolean isEmpty() {
    throw new Error("todo");
  }

  // Maps the given function on each element of this list.
  public <b> NoneOrOne<b> map(final Func<A, B> f) {
    throw new Error("todo");
  }

  // Filters the list on the given predicate.
  // The element is retained if the predicate satisfies.
  public NoneOrOne<a> filter(final Func<A, Boolean> p) {
    throw new Error("todo");
  }

  // Applies the possible function on this list.
  public <b> NoneOrOne<b> app(final NoneOrOne<Func<A, B>> f) {
    throw new Error("todo");
  }

  // Binds the given function on this list.
  public <b> NoneOrOne<b> bind(final Func<A, NoneOrOne<b>> f) {
    throw new Error("todo");
  }

  // Returns the value held in this list.
  // However, if it is empty, return the given default value.
  public A get(final Thunk<a> def) {
    throw new Error("todo");
  }

  // If this list is empty, return the given one.
  // Otherwise, return this list.
  public NoneOrOne<a> orElse(final NoneOrOne<a> els) {
    throw new Error("todo");
  }

  // For debugging
  public String toString() {
    final StringBuilder s = new StringBuilder();
    s.append('[');
    fold(new Thunk<object>() {
      public Object value() {
        return null; // Java has no suitable unit type.
      }
    }, new Func<A, Object>() {
      public Object apply(final A a) {
        s.append(a);
        return null; // Java has no suitable unit type.
      }
    });
    return s.append(']').toString();
  }

  // TEST
  public static void main(final String[] args) {
    final NoneOrOne<integer> s = NoneOrOne.one(Integer.parseInt(args[0]));

    final NoneOrOne<string> t = s.map(new Func<Integer, String>() {
      public String apply(final Integer i) {
        return new StringBuilder(Integer.valueOf(i * 123).toString()).reverse().toString();
      }
    });

    final NoneOrOne<integer> u = s.filter(new Func<Integer, Boolean>() {
      public Boolean apply(final Integer i) {
        return i < 100;
      }
    });

    final NoneOrOne<string> v = s.app(NoneOrOne.<Func<Integer, String>>one(
      new Func<Integer, String>() {
      public String apply(final Integer i) {
        return String.valueOf(Math.pow(i, i));
      }
    }));

    final NoneOrOne<string> w = s.bind(new Func<Integer, NoneOrOne<string>>() {
      public NoneOrOne<string> apply(final Integer i) {
        return i % 2 == 0 ?
          one("it's even") :
          i % 3 == 0 ?
            one("it's divisible by 3 but not 6") :
            NoneOrOne.<string>none();
      }
    });

    final Integer x = s.get(new Thunk<integer>() {
      public Integer value() {
        return 42;
      }
    });

    final NoneOrOne<integer> y = NoneOrOne.<integer>none().orElse(s);

    /*
    $ java NoneOrOne 122
    [122]
    [60051]
    []
    [3.4347832971354663E254]
    [it's even]
    122
    [122]

    $ java -classpath /tmp/ NoneOrOne 12
    [12]
    [6741]
    [12]
    [8.916100448256E12]
    [it's even]
    12
    [12]
    */
    System.out.println(s);
    System.out.println(t);
    System.out.println(u);
    System.out.println(v);
    System.out.println(w);
    System.out.println(x);
    System.out.println(y);
  }
}

// Laziness
interface Thunk<t> {
  T value();
}

// Takes an A and produces a B
interface Func<A, B> {
  B apply(A a);
}
~~~

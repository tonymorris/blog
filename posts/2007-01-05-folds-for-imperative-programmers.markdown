---
comments: true
date: 2007-01-05 16:37:51
layout: post
slug: folds-for-imperative-programmers
title: Folds for Imperative Programmers
wordpressid: 21
tags: Programming
---

Coming from an imperative language such as Java/C/C# and moving to a functional language, you will almost certainly encounter 'folding functions'. There are usually two folding functions (sometimes there are minor variants) -- one folds to the right and the other folds to the left. In Haskell, these functions are named `foldr` and `foldl`.

This first encounter with folding functions can often be quite daunting, but I propose an alternative approach. In fact, these folding functions are almost certainly quite familiar to the imperative programmer -- just without consciousness. If I had a dollar for every time I saw something like this (I'd buy every CS student a book on the lambda calculus :)):

    
~~~{.Java}
...
B accum = b;
for(final A a : as) {
  accum = f.f(a, accum);
}
return accum;
~~~


Look familiar? Not so daunting? Great, because it's a fold -- specificially, a fold to the left. It is a fold to the left because the sequence (list, array, whatever) is iterated from the left/start to the right/end. Conversely, a fold right iterates from the right/end of the sequence to the left/start.

Let's take a closer look at the 'fold left' above. The first line initialises an accumulator of type `B` to the given argument. Then the sequence (of `A`) is iterated and each element is passed to some function and its result assigned to the accumulator. Finally, the accumulated value is returned.

Have I just trivialised a seemingly complicated concept? Not at all! I have undermined its apparent complexity perhaps. On the contrary, it is the imperative language version that has complicated a trivial concept. Let's take a look at a complete and compilable example:

    
~~~{.Java}
class Foldr {
  static interface F<A, B> {
    B f(A a, B b);
  }

  static <A, B> B foldr(final F<A, B> f, final B b, final A[] as) {
    B accum = b;

    for(int i = as.length - 1; i >= 0; i--) {
      accum = f.f(as[i], accum);
    }

    return accum;
  }
}

class Foldl {
  static interface F<A, B> {
    A f(A a, B b);
  }

  static <A, B> A foldl(final F<A, B> f, final A a, final B[] bs) {
    A accum = a;

    for(final B b : bs) {
      accum = f.f(accum, b);
    }

    return accum;
  }
}

public class Folds {
  public static void main(final String[] args) {
    final Integer[] i = new Integer[]{7,8,9,42,11,13,45,54,45,46,64,74};

    final Integer right = Foldr.foldr(new Foldr.F<Integer, Integer>() {
      public Integer f(final Integer a, final Integer b) {
        return a - b;
      }
    }, 79, i);

    System.out.println(right);

    final Integer left = Foldl.foldl(new Foldl.F<Integer, Integer>() {
      public Integer f(final Integer a, final Integer b) {
        return a - b;
      }
    }, 97, i);

    System.out.println(left);
  }
}
~~~


Phew! What an effort -- pass me a beer!

We notice in the `main` method that the types of both `A` and `B` are the same (`Integer`). This is purely consequential and these types may, and often do, differ. Also notice that our function in each case is **not** commutative. That is, `f(a, b)` is not necessarily equivalent to `f(b, a)`. We know this because `a - b` is not necessarily equivalent to `b - a`. If the functions were commutative, then the result of a fold right would be equivalent to the result of a fold left. If you execute the code above, you will not get the same output for each fold. Try changing the implementation of each function to be commutative (like +) and observe the equivalent results.

Let's take a look at the Haskell equivalent at an interpreter:

    
    
    Prelude> let i = [7,8,9,42,11,13,45,54,45,46,64,74]
    Prelude> foldr (-) 97 i
    41
    Prelude> foldl (-) 79 i
    -321
    


Same outputs as the imperative code? Good, so that's folds out of the way -- easy peasey :)

By the way, yes the Haskell equivalent is supposed to be 100 times shorter -- that is the nature of a relatively expressive programming language.

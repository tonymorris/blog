---
comments: true
date: 2007-09-04 11:13:28
layout: post
slug: a-fling-with-lazy-evaluation
title: A Fling with Lazy Evaluation
wordpressid: 43
tags: Programming
---

Lazy evaluation is a core topic in computer programming that is also _widely_ misunderstood and often, erroneously trivialised. Here, I will attempt to demonstrate some of the attributes of many common mainstream programming languages; namely strict programming languages. I will also attempt to distinguish _intrinsically_ strict languages from optionally lazy languages by focusing on what intrinsically strict means. As a note, strict evaluation (or strictness) is what lazy evaluation (or laziness) is not.

Some languages are optionally lazy, including one that I use quite a lot in my work called Scala. This means I have the option of laziness, however, the default in all cases is strictness. I have to explicitly annotate laziness (=> in Scala). Some languages are optionally strict; one that I use a lot is Haskell. Laziness is the default, while strictness is explicitly annotated (seq or ! in Haskell). There are many debates about which is the correct default, but whatever the outcome of such a debate, the issue I describe next is one that demands much more attention.

Some languages are **forcibly** strict. It is these languages that I intend to focus on for the remainder of this post. I assume that my reader either understands or is at least open to just how debilitating a language is that does not support user-defined lazy evaluation in any form whatsoever. I will explicitly discuss the Java programming language, though what I describe is equally applicable to C, C# and many other languages.

In Java, the _only_ lazy constructs are:



	
  * if/else

	
  * while

	
  * for

	
  * ? : (ternary operator)

	
  * &&

	
  * ||


This is extremely limiting. To make the case clear, I will focus on a particular example of this. Consider the && function of type (Boolean x Boolean) -> Boolean. Here is a profound assertion:


> It is impossible to write && in Java yourself.


Some of you might be quick to rebut:


> 

>     
>     // Sure I can!
>     boolean and(boolean b, boolean c) {
>       return b && c;
>     }
> 
> 



This is a very crucial mistake as you will soon see.

Since the domain of the && function is so small, there is room enough to enumerate it below:

    
    a   b   a && b
    t   t   t
    t   f   f
    f   t   f
    f   f   f
    t   ⊥   ⊥
    ⊥   t   ⊥
    f   ⊥   f
    ⊥   f   ⊥


Wait just a minute!! What are those last 4 entries in the table and just WTF is that upside-down T doing!? That upside-down T represents what is called the `bottom element`, sometimes also written as _|_ when only ASCII is available. It is an extremely important part of the && function (and all functions in fact!). In Java, the bottom element is typically represented by throwing an exception (or sometimes, with `null`).

Remember, two functions are equivalent if for every element of the domain, the result of function application to that element on either function is equivalent. Here is a more formal definition for function equivalence:


> Two functions, f and g, are equivalent if the following property holds:

>     
>     ∀ x. f x ⇔ g x
> 
> 



We could rewrite the + function for `int` as follows:

    
    int sum(int a, int b) {
      return a + b;
    }


I won't list out the table for each element in the domain of +, since it is so long, so you'll either have to do it yourself, or take my word for it that the `sum` function above is equivalent to + :)

However, why can't we rewrite &&? If we look at the table for && above and select the penultimate entry, we note the following:


> 

>     
>     false && ⊥ == false
> 
> 



It is **this** entry that disallows us from rewriting &&. Look at the following function:

    
    boolean bottom() {
      throw new Error();
    }


What is the result of `false && bottom()`? It is false of course! This is because && is lazy in its second argument (sometimes; specifically, when the first argument is false). This is _impossible_ to emulate in your own Java function. If we take the earlier function that attempted (but failed) to rewrite && and call:


> 

>     
>     and(false, bottom())
> 
> 



The result of this function is ⊥ and not false, like it is with &&. Therefore, this function is **not equivalent** to &&. Further, I restate that it is not possible to write such a function in these languages. Unlike +. which is strict in both of its arguments, && is not and this makes it impossible to write a user-defined version of && and therefore, ||, if/else and ?: and importantly, many other potential functions that many Java programmers are probably not even aware of (Dynamic Programming Algorithms anyone?).

If you are ever forced to use these severely limited programming languages, you could be thankful that you have some primitive lazy constructs for doing very primitive operations (oppression), or you could be resentful for not having the expressive power of other, lazy languages (resistance against oppression) :)

Below is a session with my Scala interpreter to demonstrate that it is indeed possible to write && in Scala (`lazyAnd`), but not Java. Note that the functions (def declarations) and final variables (val declarations) below are Java functions and final variables all running in a JVM (invoked by the Scala interpreter):

    
    scala> val x = false && error("bottom")
    x: Boolean = false
    
    scala> def and(a: Boolean, b: Boolean) = a && b
    and: (Boolean,Boolean)Boolean
    
    scala> val y = and(false, error("bottom"))
    java.lang.Error: bottom
    
    scala> def lazyAnd(a: Boolean, b: => Boolean) = a && b // can't do this in Java, C#, C, etc.
    lazyAnd: (Boolean,=> Boolean)Boolean
    
    scala> val z = lazyAnd(false, error("bottom"))
    z: Boolean = false

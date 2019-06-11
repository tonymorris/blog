---
comments: true
date: 2006-11-13 21:02:42
layout: post
slug: maybe-in-java
title: Maybe in Java
wordpressid: 10
tags: Programming
---

In Haskell, there is an _algebraic data type_ called `Maybe` to represent evaluation of a partial function. That is, if you have a function, say f(x), and f(x) is defined for some x, but not **all** x, you return either:



	
  * A type Maybe that has a value available - called Just a where a is the value - representing defined function evaluation.

	
  * A type Maybe that has no value available - called Nothing - representing undefined function evaluation.



In fact, if we were to look at the source for the Maybe algebraic data type in [GHC](http://www.haskell.org/ghc/), we'd see this:

`data Maybe a = Just a | Nothing`

Not much to it, eh!? This Haskell snippet can be read as "Declare type Maybe with one polymorphic unbound variable 'a' that can be constructed as either Just 'a' or Nothing".

Consider a real Java example, [`List.get(int index)`](http://java.sun.com/j2se/1.5.0/docs/api/java/util/List.html#get(int)), which returns a type T where T is a type parameter (it once returned type Object prior to 1.5). For some Lists and some ints, this function is undefined. For example, for the list [1,2,3] and the int 7, List.get is undefined. In fact, this will throw an `IndexOutOfBoundsException` if attempted:

`java.util.Arrays.asList(new Integer[]{1, 2, 3}).get(7);`

Throwing an exception is one of our possible options for evaluation of a partial function in Java. Here are all our options available in Java:



	
  * Return `null`

	
  * Throw a compile-time checked exception

	
  * Throw an unchecked exception

	
  * Emulate continuation passing style (CPS)


I have distinguished checked and unchecked exceptions for a specific reason that I'll leave for another day and I won't go into great detail about any of these options in specific, since I am currently in the middle of documenting all this in more detail along with some basic type theory. However I will note that each of these options for evaluation of a partial function have some kind of nasty consequence. For example, the value `null` is assignable to any reference type and so can inadvertently be passed along to a method (I am sure you have seen this before):

    
~~~{.Java}
T t = f(x);
// is t null?
// the compiler doesn't force us to check!
// how does the method behave now?
method(t);
~~~


And how long has that checked/unchecked exception debate been going? Yes there is a reason it has been going for so long - in fact, it is riding on a flawed premise so it will continue on forever until its participants realise this flaw - but we'll leave that for another day and smile and nod in the meantime. :)

With a Maybe data type, we might instead write our function/method as:

`Maybe List.get(int index)`

...where undefined behaviour is denoted by returning Nothing and defined behaviour is denoted with Just T. We should also be assured that only one of these two things will occur. No more returning null, no more throwing exceptions!! How wonderful would that be!? Well, if you've ever dabbled in Haskell or type theory, you'll know immediately how great that would be - but what about Java?

An initial and naive attempt to emulate Maybe fails immediately:

    
~~~{.Java}
interface Maybe<t> {
  T get();
}
~~~


We see that we have isolated our apparent anomaly that we are trying to resolve. We must still either return null or throw an exception from the Maybe.get method, but no other method ever need do so - as long as we (hopefully) remember to check for null for any given Maybe. While a slight improvement, this of course, does not solve our problem. We are still returning null or throwing exceptions and importantly, suffering the adverse consequences of doing so (we want the compiler to fail, not have to remember to do stuff!). Instead, we need to use types to represent Just and Nothing. Let's try another attempt.

    
~~~{.Java}
interface Maybe<t> {
}

interface Just<t> extends Maybe<t> {
  T get();
}

interface Nothing<t> extends Maybe<t> {
}
~~~


Done!! Or not. There is a specific guarantee that our Haskell data type makes that our Java attempt doesn't. Specifically, if a Maybe is not Just, then it is definitely Nothing and vice versa in Haskell - this is important if we are to continue. We can see that this constraint does not hold for our Java Maybe because it can have any number of sub-types - of course - since it is an interface. We know how to constrain a type to have zero sub-types; by using a class with the `final` keyword. We know how to free a type to have infinite sub-types; without the `final` keyword or an interface. But what about constraining a type to 2 **and only 2** sub-types?

This can be achieved by exploiting a little known Java language feature - a type cannot be sub-typed if it has only private constructors **unless those sub-types are nested in the super-type**. It seems we can indeed have a Maybe algebraic data type in Java by having 2 and only 2 sub-types!


    
~~~{.Java}
public abstract class Maybe<t> {
  private Maybe() {
  }

  public static abstract class Nothing<t> extends Maybe<t> {
    private Nothing() {
    }
  }

  public static abstract class Just<t> extends Maybe<t> {
    private Just() {
    }

    public abstract T just();
  }
}
~~~


Looking good so far. We now know for sure that **every** instance of Maybe is either Just or Nothing and if it is Just, we have a value available through the get method - exactly what we ordered. As it stands though, we cannot create instances - let's expose these through public methods.

    
~~~{.Java}
public abstract class Maybe<t> {
  private Maybe() {
  }

  public static abstract class Nothing<t> extends Maybe<t> {
    private Nothing() {
    }
  }

  public static abstract class Just<t> extends Maybe<t> {
    private Just() {
    }

    public abstract T just();
  }

  public static <t> Maybe<t> _just(final T t) {
    return new Just<t>() {
      public T just() {
        return t;
      }
    };
  }

  public static <t> Maybe<t> _nothing() {
    return new Nothing<t>() {
    };
  }
}
~~~


And so there we have it - an algebraic data type in Java for evaluation of a partial function. How does our List method look now? How about this:

`Maybe List.get(int index)`

Clients of this method will now check if the return type is either Just or Nothing and in the case of Just, perform a downcast and retrieve the value. No more returning null. No more throwing exceptions. Not ever!

I'll let your imagination run wild with possibilities from here :)

As I have mentioned, I plan to go into greater detail about this problem by documenting it in detail so keep an eye out on the [Workingmouse Research](http://workingmouse.com/research) page.

Until then, I'll keep you wondering will a small piece of interesting code:


    
~~~{.Java}
import java.util.List;
import java.util.LinkedList;
import java.util.Map;
import java.util.Hashtable;

class ListMap {
  public static void main(String[] args) {
    Map<Integer, String> map =
      new Hashtable<Integer, String>();
    map.put(0, "x");
    map.put(1, "y");
    map.put(2, "z");

    List<string> list =
      new LinkedList<string>();
    list.add(0, "x");
    list.add(1, "y");
    list.add(2, "z");

    // xyz
    System.out.println(map.get(0) + map.get(1) + map.get(2));
    // xyz
    System.out.println(list.get(0) + list.get(1) + list.get(2));

    // returns null
    System.out.println(map.get(7));
    // throws IndexOutOfBoundsException
    System.out.println(list.get(7));
  }
}
~~~

---
comments: true
date: 2010-03-21 16:42:25
layout: post
slug: automated-validation-with-applicatives-and-semigroups-part-2-java
title: Automated Validation with Applicatives and Semigroups (Part 2 - Java)
wordpressid: 684
tags: Programming
---

In [a previous post](http://blog.tmorris.net/automated-validation-with-applicatives-and-semigroups-for-sanjiv/), I mentioned a small library for validating using an applicative functor pattern with a semigroup for accumulating errors using Haskell, Scala and Java programming languages. In this post, I will give a set up for an example usage, but not necessarily a complete example. This is left as an exercise and may be elaborated on in a future post.

I will start with Java. First we must decide _how we are going to accumulate errors_. I have decided to store them in a `LinkedList`. In more practical languages with a better collections library, we would probably use something else. We will use something else for Scala and Haskell (later!), or you could use the [Functional Java](http://functionaljava.org/) extension. In the meantime, we will use `java.util.LinkedList`. We start by writing its `Semigroup` implementation:


    
~~~{.Java}
public static <a> Semigroup<LinkedList<a>> LinkedListSemigroup() {
  return new Semigroup<LinkedList<a>>() {
    public LinkedList<a> append(final LinkedList<a> a1,
                                final LinkedList<a> a2) {
      final LinkedList<a> r = new LinkedList<a>(a1);
      r.addAll(a2);
      return r;
    }
  };
}
~~~



This is very straight-forward. In the previous example, I mentioned a class `Person` that is made of an age and a name. The age is an integer between 0 and 130 while the name is any string that starts with an upper-case character. Let's write these data types:

    
~~~{.Java}
// int wrapper between 0 and 130
class Age {
  private final int i;
  private Age(final int i) { this.i = i; }
  public int value() { return i; }
  public static Age age(final int i) {
    if(i <= 0 || i >= 130)
      throw new Error("out of range");
    else
      return new Age(i);
  }
}
~~~
    



And here is `Name`:


    
~~~{.Java}
// String wrapper starts with upper-case
class Name {
  private final String s;
  private Name(final String s) { this.s = s; }
  public String value() { return s; }
  public static Name name(final String s) {
    if(s.isEmpty() || !Character.isUpperCase(s.charAt(0)))
      throw new java.lang.Error();
    else
      return new Name(s);
  }
}
~~~



I won't bother writing the `Person` data type, but it is sufficient to say it will have an `Age` field and a `Name` field.

Now for validation. Suppose we have two string values; one each for age and name. We would like to check these for validation and return either one or more error messages (`String`) or a `Person`. More succinctly, we want a function with the type:

    
~~~{.Haskell}
String -> String -> Validation<LinkedList<String>, Person>>
~~~



How are we going to achieve this function? First we must say how to create an `Age` from a `String` or an error message if we cannot:

    
~~~{.Haskell}
String -> Validation<LinkedList<String>, Age>>
~~~


and same for `Name`

    
~~~{.Haskell}
String -> Validation<LinkedList<String, Name>>
~~~



We also need a function to create a `Person` from an `Age` and a `Name`

    
~~~{.Haskell}
Age -> Name -> Person
~~~


This is simple. It's the `Person` constructor.

So to sum up, we need to write a function with this type:

Arguments:




  * 

    
~~~{.Java}
F<String, Validation<LinkedList<string>, Age>>>
~~~





  * 

    
~~~{.Java}
F<String, Validation<LinkedList<string>, Name>>>
~~~





  *


~~~{.Java}
F<Age, F<Name, Person>>
~~~





  *


~~~{.Java}
Semigroup<LinkedList<string>>
~~~






Return type:


~~~{.Java}
F<String, F<String, <Validation<LinkedList<string>, Person>>>>
~~~



Of course such a function is likely to be polymorphic, rather than specifying concrete types. I recommend starting by writing a function with this type:

Arguments:




  *


~~~{.Java}
F<T, F<U, V>>
~~~





  *


~~~{.Java}
F<A, Validation<E, T>>
~~~





  *


~~~{.Java}
F<B, Validation<E, U>>
~~~






Return type:


~~~{.Java}
F<A, F<B, Validation<E, V>>
~~~



This function can be written by using the constructs mentioned in the previous post. I'm out of breath. Java is incredibly laborious.

Scala and Haskell for another time!

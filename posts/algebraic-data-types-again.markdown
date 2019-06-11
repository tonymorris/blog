---
comments: true
date: 2007-08-04 14:23:33
layout: post
slug: algebraic-data-types-again
title: Algebraic Data Types again
wordpressid: 39
tags: Programming
---

I recently watched Simon Peyton-Jones' presentation as OSCON 2007 titled, "A Taste of Haskell". As usual for Simon, his enthusiastic and very animated presentation was of high quality in both content and the way that content was presented. If you have a spare 3 hours (a little over in fact) and you are interested in learning the basics of a very powerful programming language called Haskell, I recommend that you invest the time in this presentation, which is split into [Part 1](http://blip.tv/file/324976) and [Part 2](http://blip.tv/file/325646). Be sure to keep [the slides](http://conferences.oreillynet.com/presentations/os2007/os_peytonjones.pdf) available as well, since a lot of the time, the camera is focused on Simon and not on the slide that he is discussing.

During the presentation, the audience were asking questions of Simon as he was presenting Algebraic Data Types (ADTs), which are written in Haskell using the `data` keyword. As Simon pointed out, ADTs are a fundamental part of writing Haskell. I felt that members of the audience were perhaps considering the concept of an ADT to be more complicated than what it really is. It truly is quite simple and in this post, I hope to try a new tact to explain this very simple concept. As Simon also pointed out, ADTs are important, powerful and fundamental to programming, but I don't expect this to be obvious at first. First, let's understand exactly what an ADT is. If the following confuses you and you prefer Simon's (or some other) approach, then please ignore it. I intend to use a (probably) familiar language to demonstrate what an ADT is.

In Haskell, ADTs are _closed_. That is, at the point of declaration you specify all the constructors of the ADT and it is not possible to dynamically add more constructors for that type. For example, the Haskell Maybe data type is written as follows:

    
~~~{.Haskell}
data Maybe a = Just a | Nothing
~~~


Therefore, the Maybe data type has 2 **and only 2** constructors. It is the "and only 2" that makes this data type a closed algebraic data type.

If we consider a Java or C# `class` that can somehow enforce 2 **and only 2** subclasses, then we could emulate closed algebraic data types in these languages. Indeed, it is possible to do so, however, you have to get really quirky with the language at hand. [I have written about this before](http://blog.tmorris.net/maybe-in-java/), but instead of using this trick, I am going to invent a new keyword for these languages called `klass`.

A `klass` declaration is always abstract and all of its subtypes are always specified in the same source file and they are always implicitly final. An attempt to subclass a `klass` outside of its own source file or any subtypes of a `klass` is a compile-time error. Furthermore a `klass` cannot declare anything at all in its body -- it simply takes on the default constructor and nothing more is permitted.

Given this new keyword, we can write the Haskell Maybe data type as follows:

    
~~~{.Java}
class Maybe<A> {

}

class Just<A> extends Maybe<A> {
  final A a;
  Just(A a) {
    this.a = a;
  }
}

class Nothing<A> extends Maybe<A> {

}
~~~



To reiterate, no other classes may subclass `Maybe`, `Just` or `Nothing` and there is a single type parameter that I have called `A` (and Haskell calls `a` by enforced convention). Therefore, if a function or method is passed an instance of `Maybe<A>`, we can be **certain** that it is either a `Nothing<A>` or a `Just<A>` and in the `Just` case, we might wish to access that `A` (which is a field called `a`).

Indeed, we might write code that is similar to the following:

    
~~~{.Java}
X someFunction(Maybe<A> m) {
  if(m instanceof Just) {
    Just<A> j = (Just<A>)m;
    foo(j.a);
  // this definitely holds, but see description below
  } else if(m instanceof Nothing) {
    bar();
  }
}
~~~


This code is effectively _pattern matching_ (another topic that Simon talked about and the audience seemed unnecessarily confused). In the second `if` statement I have ensured _non-overlapping_ pattern matching, which is a good practice that I follow. That is to say, the order of my `if` statements is not relevant to the outcome of the function. You might recall that Simon said that pattern matching occurs "top to bottom", however, I prefer not to rely on this by not overlapping patterns. If I'd left out the second `if` condition as a simple `else` block, then I have effectively said, "and everything else" and the behaviour of the function would not change. However, this pattern overlaps with the `Just` case and the behaviour of the function would be affected if I swapped their order. This notion of "and everything else" is denoted in Haskell pattern matching with an underscore or sometimes, a variable to bind to.

The code above, written using Haskell pattern matching would look like this:

    
~~~{.Haskell}
someFunction :: Maybe a -> X -- remember, this is a type declaration
someFunction (Just a) = foo(a)
someFunction Nothing = bar
~~~



Simon talked about pattern matching as "decomposing" a data type. You can see in the earlier code that I have pulled apart the `Maybe` into its components of `Just` or `Nothing` and I have "decomposed" the components of the `Just` further, by getting the `A` out of it. This is what Simon was referring to when he was talking about decomposition of a data type using its constructors.

Of course, in Haskell, you can create all sorts of data types and the `Maybe` data type is very common. There is another called `Either` that looks like this:

    
~~~{.Haskell}
data Either a b = Left a | Right b
~~~


It is almost like `Maybe` except that both constructor cases take an argument of any type.

Here's the fun part :) If we take a look at all the excitement with "non-nullable types" using the type system in [Java](http://www.disi.unige.it/person/AnconaD/FTfJP06/paper03.pdf) and [C#](http://codebetter.com/blogs/patricksmacchia/archive/2007/07/25/i-want-non-nullable-types-in-c-4.aspx), then we see that this is **exactly** what the `Maybe` data type does in Haskell, also using the type system. In fact, imagine "exceptionable types" in these languages, where returning one of these "non-exceptionable" types from a method cannot throw an exception. This is **exactly** what the `Either` data type above does also using the type system, since we can return "either the value to be returned or the exception that was thrown". Furthermore, the `Maybe` and `Either` data types are one line of Haskell code and just 2 of many possible data types. In other words, and as is usual for previous industry trends, the excitement around Java/C# non-nullable types, is just a fundamental library feature of Haskell that has been there for years. How about that eh? :)

The next time a Haskeller just smiles and nods at a Jumping Java Joey or Shaggin' C Sharpy who is all excited about non-nullable types (or any "revolutionary" language feature in fact), you'll now know why since both `Maybe` and `Either` are so trivial and fundamental, that they are included in the default import of the Haskell standard library (like `java.lang.*`).

I hope you too can smile and nod after reading this post and understand Algebraic Data Types (and Pattern Matching) :)

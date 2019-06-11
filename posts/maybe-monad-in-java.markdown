---
comments: true
date: 2006-12-18 14:06:35
layout: post
slug: maybe-monad-in-java
title: Maybe Monad in Java
wordpressid: 16
tags: Programming
---

As [I have shown previously](http://blog.tmorris.net/maybe-in-java), the problem of partial function in Java is not easily solved. The solution of the Maybe algebraic data type, while definitely superior to existing options, is cumbersome to implement and requires some functions defined over the type (isJust, isNothing, etc.) in order to be complete. As some point out, there is a preference for continuation passing to prevent the need for a cast (even though this cast would be hidden). This prompted me to provide a _[more complete, yet still incomplete](http://blog.tmorris.net/revisiting-maybe-in-java)_ solution.

It is also worth noting that the solution with continuation passing [will not work on the Sun VM due to its lack of tail call elimination](http://bugs.sun.com/bugdatabase/view_bug.do?bug_id=4726340). i.e. some concessions must be made somewhere - manual compiler optimisations if you will :) There were also some observers who [didn't quite get it](http://programming.reddit.com/info/qt96/comments/cqx5q). Nevertheless, many of us will go on using null/exceptions in Java or their own implementation of Maybe in Java (feel free to steal mine if you like), while others will use Haskell's Data.Maybe, Scala's Some/None, Nice's option types or whatever.

All this aside, I am writing this post to introduce the Maybe Monad in Java, not to demonstrate anything meaningful or clever. Rather, I wish to simply point out to Java programmers what this abstract and apparently mystical notion of a monad - taken from the mathematical branch of category theory - really is. We now know that exceptions are **only ever** used for representing a partial function (even for I/O!) and we know that the Java programming language has built-in language and API support for exceptions. Let's just revisit what the concept of a 'partial function' means exactly. It means that "there exists"[_see note 1_] some x take from our _universe of discourse_ (U) such that f(x) is undefined.

Sound scary? It's not really. Suppose we have a data type, `int`. The size of our universe of discourse is finite; it is 2^32. We can represent this as |U| = 2^32. The notation f(x) represents some function with x applied. Having a universe of discourse of finite size implies that we can do an exhaustive proof for behaviour of the function, though not necessarily in a reasonable amount of time.

Let's call a function 'divide' and x can be two ints, a and b. The size of our universe of discourse of our arguments is 2^32 * 2^32 (i.e. 2 ^ 64) because that is the total number of combinations for the values of its arguments that are available. We can say that for our function, divide, it holds that for all[_see note 2_] a and for b = 0, the function is undefined. That is, regardless of the value of a, if the value of b is 0, the function is undefined:

    
~~~{.Java}
int a = ... // anything
int b = 0;
divide(a, b); // undefined
~~~


If we try to use this function with b = 0, we will observe an exception (ArithmeticException). If we were using reference types, we could write our own divide function that instead returns `null` or returns `Maybe<Integer>` and Nothing for the case of an undefined function application.

Have we got it? Great! :) The Maybe Monad is used for defining a partial function that might call 2 or more partial functions; if any of those functions called are undefined, then our function that we are writing is undefined. This might be called, "_threading partial function through a computation_". We could write it in pseudo-code:

    
~~~{.Haskell}
f x = if isNothing (g x)
            then return Nothing
            else if isNothing (h (g x))
              then return Nothing
              else return maybeToJust (h (g x))
~~~


Very cumbersome to write and I sure hope that the function f doesn't need to use more than 2 partial functions (g and h) in order to define! It would be great if you could write something similar to this instead...

    
~~~{.Haskell}
f x = h (g x)
~~~


...and not have to worry about "_threading partial function through the computation_". Those familiar with Haskell, or those who are trying to understand monads in a Haskell context, should hopefully recognise the latter syntax as _do-notation_. Those who are not familiar with do-notation can just know that Haskell provides language support for this notion of threading computation, represented by the more familiar term, monads. This support is provided with a language keyword called `do` (**very** different to do/while in an imperative language!).

Anyway, this post is getting a bit long and I still haven't introduced Java's monad for threading partial function through a computation. Luckily, it is not very long to write so without further ado, I introduce it as follows:

    
    
    throws
    


"WTF!!? You mean I have been using monads all along!!? How can that be!?" Well, it has been stated more than once, that many people [have already used monads](http://sigfpe.blogspot.com/2006/08/you-could-have-invented-monads-and.html), even if they didn't call it that. In recent years, fundamental computer programming concepts have often been confused with euphemisms like "design patterns" or some such, so rather than speculate at an analogy that might be familiar, I will instead turn focus on the Java `throws` keyword.

Consider the following rough Java code:

    
~~~{.Java}
T foo() throws IOException {
  H handle = openFile(); // this method declares throws IOException

  try {
    maybeDoSomeStuff(handle); // this method declares throws IOException
    // then
    return someFunction(handle); // this method declares throws IOException
  } finally {
    close(handle); // this method declares throws IOException
  }
}
~~~


We see quite clearly that if any of the invoked methods are undefined then our entire method (`foo`) is also undefined - by returning with an exception instead of a type T. Sound familiar yet? Since we know that exceptions are only ever used to represent partial function, then we can infer that the `throws` keyword is indeed a (poor man's) Maybe Monad in disguise. I note at this point that the Maybe Monad is one of a possible squillion monads that are used in every day programming; _even_ in the weakest of  programming languages. Here are a couple that are used very often though not necessarily with built-in language support:



	
  * The list monad - building up a list or threading a list through a computation

	
  * The state monad - passing "state" through a computation



The lesson from this somewhat lossy representation of a monad - that is hopefully familiar to most people - is that monads are not anything overly complicated or abstruse despite initial appearances. It requires very little brain power to digest the concept coming from a nil set of knowledge, but unfortunately, it appears to take a considerable effort if the subject has been "tainted" with ill-conceived presuppositions such as imperative programming languages.

Be assured, the lesson is brief.
Be warned, the lesson is very self-confrontational.

[1] This is called existential quantification and is often denoted by the symbol ∃
[2] This is called universal quantification and is often denoted by the symbol ∀

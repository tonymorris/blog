---
comments: true
date: 2008-06-28 18:28:15
layout: post
slug: just-what-the-funk-is-a-functor-anyway
title: Just what the funk is a Functor anyway?
wordpressid: 92
tags: Programming
---

[Runar](http://apocalisp.wordpress.com/) recently made mention of these so-called Functors and Monads in [his excellent post about concurrency/actors in Java](http://apocalisp.wordpress.com/2008/06/18/parallel-strategies-and-the-callable-monad/). There are all sorts of tutorials out there for understanding what a Monad is, however, I am of the opinion that one must first understand what a Functor is. This is because it is less complex and more general (all monads are functors plus more).

The Java Posse also made a couple of false claims about Runar's article, but one in particular needs addressing. This is the notion that a functor is applicable to "Functional Programming". This is false. A functor is applicable to _programming_. You really, really need to understand what a functor is, if you're ever going to be skilled in _any type of_ programming; even if it's just Java web applications for the rest of your life. This concept does not exist 'on the other side of the fence' in the imaginary 'functional programming world'. These suggestions are understandable of course and I don't intend to labour the point -- only make this point clear, since these apparent divides seem to pop up often. On with the story...

In Java, we have an interface called `[CharSequence](http://java.sun.com/javase/6/docs/api/java/lang/CharSequence.html)`. A few things can be said about `CharSequence`. How about:


> 
All implementations guarantee that for some integer `n`, then if `n >= 0 && n < length()`, then `charAt(n)` will not throw an exception.




We might call this a _law_ about `CharSequence` that all implementations must satisfy. We could even find other laws that must satisfy about implementations of the `CharSequence` interface.

A Functor is an interface and it has two laws. Sadly though, this interface cannot be expressed using Java's type system -- it is simply not clever enough. We must use a hypothetical Java to express it. First though, we have to make up for Java's missing first-class functions by using an interface. This is easy:

    
~~~{.Java}
interface Function<A, B> {
  B f(A a);
}
~~~


OK, so this a just an interface like any others. It needn't satisfy any laws; it just takes an argument (A) and returns (B). We could write bazillions of implementations of this interface in Java. We're going to need this interface to write the `Functor` interface.

Now, the missing part of Java is called _higher-kinds_ -- a term you needn't concern yourself with too much. I hope this hypothetical syntax is enough to make the point clear:

    
~~~{.Java}
interface Functor<E<_>> {
  <A, B> E<b> fmap(Function<A, B> f, E<a> fa);
}
~~~



So the foreign part here is `E<_>`. Consider E to stand for 'Environment'. It is a type parameter that takes yet another type parameter (which is why it is denoted E<_>). This means I could use `List` or `Set`, but not `Integer` as the type parameter value for E, since `List` and `Set` take another type parameter while `Integer` does not. You might think of the `Functor` interface as 'applying the given function within the environment'.

We might write an implementation like this:

    
~~~{.Java}
class ListFunctor implements Functor<list> {
  public <A, B> List<b> fmap(Function<A, B> f, List<a> list) {
    // todo: apply the given function on all elements in 'list' and return the result
  }
}
~~~



In general discussion, we would call this _the list functor_, simply because the value for the environment has been applied. In Runar's article, he was talking about the `Callable` functor; yet another environment. There are many possibilities for the environment here; `List` and `Callable` are but two.



#### The Laws



Let us not forget the laws :) Just like all implementations of `CharSequence` have laws to obey, so do all functors. They are called _identity_ and _composition_.



##### Identity



Remember the `Function` interface? Here is an implementation:

    
~~~{.Java}
class Identity<a> implements Function<A, A> {
  public A f(A a) { return a; }
}
~~~



If we are given an implementation of `Functor` -- which I will call `f` -- then it must satisfy `f.fmap(new Identity(), x)` is equivalent to `x` _(for any value of x)_. So that's the identity law out of the way. Mapping _the identity function_ across a functor yields the same value.



##### Composition



The law of composition is a little less friendly when it comes to expressing it in Java so I will refrain :) While it is an important part of making up the concept of a functor, it may be best to defer this final piece of the puzzle for now.

For completeness, I will state the law using a shorter syntax, but if this is foreign or worrying, then please ignore it: `f.fmap(a compose b, x)` is equivalent to `f.fmap(a, f.fmap(b, x))` _(for any value of a, b and x -- a and b are `Function` objects)_.

That's all there is to a Functor. If you use the `map` function on lists or perhaps the `map` method on `scala.Option`, then you are using one specific instance of a functor, since these functions/methods satisfy the conditions for a functor. Even throwing an exception is using a specific functor! Perhaps that can be explained another time :)

I should also add that when referring to a functor (unqualified), we are referring to a _covariant_ functor. Other types of functors include contra-variant and invariant functors. We can talk about those another day :)

Next: Just What the Flip is a Monad?

Questions?

---
comments: true
date: 2010-12-28 19:39:08
layout: post
slug: nothing-returns-anything-ever
title: Nothing returns anything, ever!
wordpressid: 866
tags: Programming
---

I once had the following discussion with someone regarding a Java project:



> 
Him: Once we did a project and we didn't return anything from a method, ever (i.e. `void`) It went very well.
Me: So everything was in CPS transform then so that you could simulate it?
Him: No, we didn't ever return anything ever!
Me: So uh, it was uh, in CPS transform then, since er... you know...
Him: No! Nothing. Return. Nothing. Ever. Void. Always!
Me: Er yeah right uh huh.




This person operates under the illusion that I am in an intellectual battle with him, so I can excuse the somewhat awkward discussion. My efforts to destroy this destructive illusion have been repeated failure to date. That's a side-story.

It occurred to me later, thanks to Edward Kmett, that perhaps it wasn't a CPS transform but using exceptions instead! I doubt that was the case :) I'm also reasonably confident this discussion was _argumentum ad ignorantiam_ -- my converser doesn't know what CPS transform means (so it can't possibly be true!), but that's cool -- let's not dwell on it. Instead, let's have a look how you can simulate returning a value without actually returning a value using continuation-passing style (CPS)!

We may conjure up both simple and elaborate examples of functions that actually return a value:


    
~~~{.Java}
Integer wibble(String s, double d)
Swizzler function1(int i, String s, Swazzle z)
~~~



I am going to use the trivial example (top one), but I hope you'll be able to extrapolate to swizzlier cases.

First, note that Java methods may perform side-effects willy-nilly. Passing in a `String` and a `double` are not the only values that the function may access. The function may print to standard output, use variables in any scope, read/write files, the network and so on. This changes things a little (read: a lot) with regard to how we look at a function, so perhaps this is why this technique has apparent (read: apparent) merit.

Suppose a very simple interface:


    
~~~{.Java}
interface To<a> {
  void to(A a);
}
~~~



Quite simply, this function takes a polymorphic (aka generic) value and performs a side-effect, perhaps using this value. It is important to point out that this polymorphic interface may be modelled differently e.g. it may not be polymorphic:


    
~~~{.Java}
interface Stringer {
  void stringer(String s);
}
~~~



This interface is exactly equivalent to `To<String>` so I'm also trusting that you'll extrapolate my generic example to other possibilities.

Now, instead of returning a type e.g. `Integer` in the `wibble` function, we may return `void` with an additional `To<Integer>` argument. We can do this for every single function that would otherwise return a value!

How does that work then? Simple really. We compute the `Integer` as we normally would in the body of `wibble` and instead of returning it (we can't of course), we pass it to the `to` method of the given `To<Integer>` instance.


    
~~~{.Java}
void wibble(String s, double d, To<integer> t)
~~~



And so on it goes, for every method.

Essentially, this results in a function that, instead of returning a value of type T, returns a value of the type `(T => void) => void` where `(T => void)` is represented by the `To` interface and is of course, in the position of a method argument where that method returns `void`. This is exactly equivalent to a specific example of a continuation. We are doing a CPS transform to simulate returning a value!

To summarise the formula:



> 
For a method that would normally return a type T, we can transform it in such a way to accept an argument of type To<T> and returns void.




As for the merits of doing this, well I'll leave that for another battle :) Further, there are some interesting properties about continuations, in particular, [they are a monad](http://blog.tmorris.net/continuation-monad-in-scala/), perhaps even [the mother of all monads](http://blog.sigfpe.com/2008/12/mother-of-all-monads.html), but we'll leave all that for another day too.

Edit: Per comments, I think a clarification is in order. Doing this is a **very very bad idea** on the JVM (without tail-call elimination -- note that the IBM implementation only does direct TCE). Doing this results in a significant performance degradation and increased syntax **for zero benefit**. I didn't bring this up because I was never going to get that far in the original conversation that I mention. **Beware.**

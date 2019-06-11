---
comments: true
date: 2007-01-01 09:32:23
layout: post
slug: strong-type-systems
title: Strong Type Systems
wordpressid: 20
tags: Programming
---

I have been reading articles lately that make a case for or against 'strong type systems' by referring to artifacts that purport the exact opposite of a strong type system such as Java, C/C++, C# or a few other weak type systems. This is unfortunate, since the remainder of the article becomes immediately invalid within the first two or three sentences and I doubt that the author is even aware of the fact that they are wasting their own time while at the same time, misleading any potential readers. Quite often, these articles have trendy or appealing writing styles, such as "[Parabola](http://steve-yegge.blogspot.com/2006/12/parabola.html)", which attempts to universally refute the legitimacy of type safe (T.S. in the article) systems by making a rather clever, albeit erroneous, metaphor with a scenario in an airport.

[There](http://eureka3d.com/blog/?p=21) [are](http://www.cs.oberlin.edu/~jwalker/nullObjPattern/) [countless](http://today.java.net/pub/a/today/2004/12/10/refactor.html) articles that talk about the problems with `null`, but is this a problem with type systems? No, **it is a very poor solution to work around one of the fundamental contradictions that plagues imperative programming**. That is, even if we must keep this contradiction and imperative programming for some reason (which we don't), there are better solutions than `null`. So this leaves a problem for any author who wishes to provide a higher understanding for their readers; do they explain all the problems with `null` in more formal, concise and correct terms knowing that the whole problem disappears with a better foundational grounding (eliminating imperative programming), or do they simply present some alternatives that may be unfamiliar and hope for initiative on the part of the reader who will use (the ever so common) _gut instinct_ to _feel_ that they have a superior solution? A horrible problem that is thankfully not mine.

Just what denotes Java et. al. having a weak type system? On what basis is this statement made? The answer to this question is not trivial by any means and answering it requires a reasonable amount of foundational mathematics knowledge, which leads to specialisation in type theory --  both of which not many software developers have, nor have the initiative to acquire (this is sad :(). In any case, a brief attempt can be made. There is no doubting that these weak type systems disallow a certain amount of expressivity and force a rigid and sometimes unreasonable method of development. That is **not** to say that dynamic typing is the solution. Just like Dawkins said that since the beginning of life is so improbable as to be impossible (on the surface), then the existence of a supernatural creator is **not** the only alternative solution -- referring to Darwinism. Whether or not you believe in a supernatural creator, it would be obscene and somewhat ignorant to immediately jump to such a conclusion without rational reason --  such as critically analysing alternatives. Likewise, it is obscene to jump to the conclusion that dynamic typing solves the problems of (weak) existing type systems without considering alternatives, such as expressive type systems.

Using Java et. al., it is impossible to express certain fundamental programming concepts and many more concepts that are specific to the problem at hand. One such fundamental programming concept is the _monadic bind computation_. Before I start going into detail about this, I am compelled to first dispel a very common myth. Many 'programmers' believe that monads (and therefore, monadic bind) are not relevant to their problem, since their language does not support such a concept directly. Few programmers actually realise that they are in fact using monads, just without language support and often in a somewhat contrived manner --  a result of the lack of formality in reasoning. In fact, I postulate that in my time working on IBM WebSphere 6 and the IBM JDK 1.5, I saw somewhere in the order of hundreds of monads littered throughout thousands of lines of code. Furthermore, [Java/C# has one particular monad built right into the language](http://blog.tmorris.net/maybe-monad-in-java). Providing concrete examples is worthy of a topic on its own, so I will instead take the liberty of claiming that if anyone were to send me their 2^gazillion LOC application, I would be capable of finding one monad per one thousand LOC -- worst case scenario. At least, please assume so for now. **Yes, monads are extremely important and common to programming, even if you don't ever know it explicitly**. Great, glad that is out of the way :)

We cannot express monadic bind with these weak type systems because they do not have _higher-order types_. What is a higher-order type? [A few people with much more time, specialisation and knowledge have done a better job than I am at least willing and probably capable, of doing](http://www.google.com/search?q=higher-order+types). Instead, I will attempt to demonstrate what happens when you attempt to generalise monadic bind with a weak type system. Haskell is a purely functional language that _does_ have higher-order types. If we ask a Haskell interpreter for the type of bind (>>=), we get the following:

    
    
    Prelude> :type (>>=)
    (>>=) :: (Monad m) => m a -> (a -> m b) -> m b
    



This is akin to a Java/C# interface (called `Monad`) containing one function (>>=) or 'bind', however, we will note that **there is no analogy for this expression in a weak type system**. This fact, along with many others, seems to be quite a hurdle in grasping the very concept of the bind computation itself. If we examine this type expression, we could say that the bind function (>>=) takes two arguments:



	
  1. a monad with a type parameter 'a'

	
  2. a function from 'a' to a monad with a type parameter 'b'


The function returns a monad with a type parameter 'b'. 'Implementing this interface' would make the class itself an instance of `Monad` and so itself can be used as the function arguments. This is one of many things that _higher-order types_ gives us.

Let's attempt this with a weak type system, Java. First, we acknowledge that Java has no first-class functions (cannot pass functions as arguments), but we can loosely emulate it (good enough for our purposes) with a single interface:

    
~~~{.Java}
interface F<X, Y> {
  Y f(X x);
}
~~~


Let's attempt to write the `Monad` interface:

    
~~~{.Java}
interface Monad<A> {
  <B> Monad<B> bind(Monad<A> ma, F<A, Monad<B>> f);
}
~~~


Looking good so far. But don't be fooled -- there is a huge problem. Namely, that the interface cannot be implemented correctly. Keeping with tradition, let's attempt to write the [Maybe monad](http://blog.tmorris.net/maybe-in-java):

    
~~~{.Java}
final class Maybe<A> implements Monad<A> {
  // BZZT! WRONG!
  Maybe<A> bind(final Maybe<A> ma, final F<A, Maybe<B>> f) {
~~~


We hit our first problem, can we work around it? In Java, we have co-variant return types, but we do not have higher-order types. This means that we can keep our return type declaration, but not the types of our arguments -- they **must** match the interface. Then, we can resort to casting -- an abomination that indicates a flaw of the type system.

Let's try again:

    
~~~{.Java}
final class Maybe<A> implements Monad<A> {
  public <B> Maybe<B> bind(final Monad<A> ma, final F<A, Monad<B>> f) {
    if(ma instanceof Maybe) {
    ...
~~~


This solution works, but we have to admit to having to work around the type system by resorting to casting. As a result, I will no longer call it a solution, but instead, it is a work around --  a flaw in the expressitivity of the type system. The lack of higher-order types in common type systems is one of many problems with quite devastating consequences. In attempting to model a fundamental computational concept, we find that the type system of Java (and many other programming languages with weak type systems) is not a sound premise from which to extrapolate conclusions about type systems in general. If we were to do so, we would be making a fundamental mistake. In fact, this particular problem is one that dynamic typing appears to solve. But don't let these superficial appearances delude you. A stronger type system is the correct solution.

Here are some conclusions from these findings:



  
  * Higher-order types are fundamental to a strong type system

  
  * Java/C/C++/C# et. al. do not support higher-order types, therefore, do not have a strong type system

  
  * Monadic bind is a relatively trivial, yet extremely common programming concept

  
  * Monadic bind cannot be easily expressed in common weak type systems

  
  * Dynamic typing is not the only apparent solution to resolving the problems of weak type systems



Here are some questions to ponder:

  
  * What if we tried to model a concept that is not as trivial as the bind computation? Does the difficulty of expression grow linearly to the complexity of the concept?

  
  * What other concepts are supported by strong type systems but not our common weak type systems?

  
  * Can we come up with other examples of common, trivial concepts that cannot be expressed because these weak type systems are missing some other feature besides higher-order types?

  
  * **Most importantly, what does a comparison between a type system that exemplifies sound, strong and expressive type safety with dynamic typing (or no typing) give us?**


It is this latter question that needs to be explored before drawing any potentially misleading conclusions about type systems. **It is a crucial mistake to make a comparison to weak type systems that do not in any way exemplify soundness, as anything else but unsound examples of type systems. The implication that dynamic typing is a correct solution is a blind-man's fallacy.**

A compilable solution follows:

    
~~~{.Java}
interface F<X, Y> {
  Y f(X x);
}

interface Monad<A> {
  <B> Monad<B> bind(Monad<A> ma, F<A, Monad<B>> f);
}

final class Maybe<A> implements Monad<A> {
  public <B> Maybe<B> bind(final Monad<A> ma, final F<A, Monad<B>> f) {
    if(ma instanceof Maybe) {
      final A j = ((Maybe<A>)ma).just();
      if (j == null) {
        return new Maybe<B>();
      } else {
        final Monad<B> b = f.f(j);

        if(b instanceof Maybe) {
          return (Maybe<B>)b;
        } else {
          throw new Error("Just because we don't have higher-order types," +
            "doesn't mean we start doing silly stuff");
        }
      }
    } else {
      throw new Error("I said stop doing silly stuff! Please!");
    }
  }

  // Let's just hack for now.
  // null denotes 'Nothing'.
  // Many programmers will be familiar
  // with this idiom (did I say hack?)
  // anyway.
  //
  // A more complete solution is provided at
  // http://blog.tmorris.net/revisiting-maybe-in-java
  private final A a;

  public Maybe() {
    this.a = null;
  }

  public Maybe(final A a) {
    this.a = a;
  }

  public A just() {
    return a;
  }
}
~~~

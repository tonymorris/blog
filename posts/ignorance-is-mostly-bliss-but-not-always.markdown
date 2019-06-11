---
comments: true
date: 2007-02-09 13:30:51
layout: post
slug: ignorance-is-mostly-bliss-but-not-always
title: Ignorance is mostly bliss, but not always
wordpressid: 23
tags: Programming
---

I have been using [Scala](http://www.scala-lang.org/) for the past few weeks to write a web framework that 'fixes' the convoluted Java Servlet Specification. I have succeeded in doing so and I have a prototype running on Apache Tomcat right now as I write this post, however I didn't get to this point easily; not without first having to do such things as:

* write a lazy cons list **without** any methods declared on the type -- rather, functions over the type that are declared in a Scala _object_ (roughly analogous to Java static methods)


* conceding to using mutable state/destructive update at some points -- particularly converting strict and/or side-effecting Java types to nice, lazy, pure types


* missing out on the syntactic niceties that monads provide -- Scala does not support higher-order kinds


Scala is what I consider a very pragmatic (to steal a buzzword from the hype generators) approach to software development as it exists today. It satisfies the incompetent assertions of the illegitimate authority (aka management) -- "you must use Java (et al.)", while still allowing a reasonably concise expression and programming paradigm e.g. Scala has pattern matching, case classes, variance annotations, higher-order functions and implicit defs (similar to Haskell type-classes). These features alone dramatically reduce the amount of repetitious work that you otherwise do in more mainstream languages by one of those crazy orders of magnitude that would never be believed by a mainstream programmer and so could easily be explained away as nonsense with support from other ill-informed peers (those who know what I mean, know what I mean).

I had read about [CAL](http://labs.businessobjects.com/cal/), but didn't ever give it much consideration until today. And it is today that I realise that my blissful ignorance has cost me. CAL is very _Haskell-like_ with full support for using Java types (just as Scala does) and compiles straight to the JVM (just as Scala does) -- making up for the deficiencies of the Java runtime compiler using techniques such as tail recursive call elimination (again, just as Scala does).

However, upon further reading (I admit to not having used it in great detail just yet), CAL does more -- much more. It is lazily evaluated and it is pure functional. It has higher-order kinds [and we know how important that is](http://blog.tmorris.net/strong-type-systems/). It has monads and even the `Monad` data type subtypes the `Functor` data type! Not even Haskell does that (though I'm sure things would be different given hindsight ala CAL)! Type parameters are covariant, given its pure functional nature and it makes expressing strictness much easier than it does in other lazy languages (apparently). Of course, this is all potentially a reiteration of hyperbole, since I've yet to try it out -- but it does look promising, especially given that it has now been released under a BSD licence.

The point is, my ignorance (of CAL) has cost me, since I have a well thought-out, very usable web application framework (sorry [HAppS](http://happs.org/)) that has been written in a possibly (probably) inferior language. Here I was thinking I had the best of both worlds -- satisfying the pointy-haired boss, while still getting on with writing software. Though, it could be worse, **much** worse -- I could have written it in Java/C# like many other participants in this circus are doing.

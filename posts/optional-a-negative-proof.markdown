---
comments: true
date: 2010-06-20 13:57:14
layout: post
slug: optional-a-negative-proof
title: Optional a -> a (negative proof)
wordpressid: 779
tags: Programming
---

_A loose follow-on from [Java Trivia](http://blog.tmorris.net/java-trivia/)._

I run a weekly Haskell session at my place of employment. We are just beginning. The intention is not so much to learn Haskell, but to learn deeper programming concepts so that we can apply them regardless of language -- though to the extent that various languages permit.

Recently, we were discussing algebraic data types and we invented our own:

    
    data Optional a = Full a | Empty deriving (Eq, Show)


This is equivalent to:



	
  * [fj.data.Option](http://functionaljava.googlecode.com/svn/artifacts/2.23/javadoc/fj/data/Option.html) for Java

	
  * [Data.Maybe](http://www.haskell.org/ghc/docs/6.12.2/html/libraries/base-4.2.0.1/Data-Maybe.html) for Haskell

	
  * [scala.Option](http://www.scala-lang.org/docu/files/api/scala/Option.html) for Scala

	
  * [option](http://msdn.microsoft.com/en-us/library/dd233245.aspx) for F#

	
  * [module Option](http://ocaml-lib.sourceforge.net/doc/Option.html) for OCaml

	
  * [Option](http://www.standardml.org/Basis/option.html) for SML

	
  * [option](http://www.lix.polytechnique.fr/coq/stdlib/Coq.Init.Datatypes.html#option) for Coq


We were writing a function with this type

    
    Optional a -> a -> a


when a member of the audience said, "Why not just use `Optional a -> a`?" to which I responded, "That's not possible to do in a consistent way." That is, not only we shouldn't, but we cannot do it, no matter what! By consistent here, I mean a total, terminating function. Telling programmers that something is not possible is often a little hasty, even if it is true, so I side-stepped any further discussion on this matter and carried on, promising further explanation, which follows.

Here I will prove it is not possible by exploiting the Curry-Howard Isomorphism. It is not intended to be deep or technical, only to display some of the possibilities of using "types as propositions, programs as proof" -- to paraphrase the intention of C-H Isomorphism.

The essence of C-H is quite simple. If you write a type signature, you have also written a logical proposition. If you find at least one function that satisfies this signature, you have proven that proposition. Conversely, if you have a program, its existence proves its type (whether the programming language explicitly supplies that type or not). This program for a type is called an _inhabitant_ of the type. Some types are uninhabited, for example: `a -> b`. That's because this proposition is not true.

To state it logically, `forall a b. a implies b` is a false statement. You can determine this by a simple truth table:

    
    a b a->b
    0 0 1
    0 1 1
    1 0 0 <-- inconsistency
    1 1 1


In [a previous post](http://blog.tmorris.net/java-trivia/), I demonstrated a function that was _once-inhabited_ using (a subset of -- _see the rules_) Java. I knew it was once-inhabited before I gave the puzzle, meaning that every answer given should be the same (extensionally equivalent). Once-inhabited functions are very interesting, but not here -- since we are looking for either:



	
  * At least one inhabitant (proof which I have claimed does not exist)

	
  * The absence of an inhabitant (proof negative)


So how can we prove or disprove `Optional a -> a`?

My claim is that it is uninhabited. You can disprove this claim by finding an inhabitant. However, the inability to find an inhabitant does not disprove the proposition -- after all, we might just be having an unimaginative day! I am assuring you for now, that you will find no such inhabitant. Now I will prove that you won't.

We first ask the question, what exactly is `Optional a`? We can provide a data structure of equivalence by exploiting the [catamorphism](http://en.wikipedia.org/wiki/Catamorphism) for the data type:

    
    type Optional a = forall x. (a -> x) -> x -> x


This data type is isomorphic (of the same one form) to our previous one and you can determine this by passing in the two constructors of the earlier data type to this isomorphic data type to produce an identity function:

    
    let cata :: (a -> x) -> x -> x
    cata Full Empty == id


Side note: if you're interested in doing the same for a Haskell list, see the `foldr` function denoting the list catamorphism (`foldr (:) [] == id`).

Therefore, our proposition to prove/disprove is now rewritten _(remember that `->` is right-associative)_:

    
    forall a x. ((a -> x) -> x -> x) -> a)


Using this, and the truth table for logical implication, we can find an answer by another truth table. Let us first assign some of the parts of this proposition to values for brevity (s being the proposition under investigation):

    
    p = a -> x
    q = x -> x
    r = p -> q
    s = r -> a


Let us now draw the truth table:

    
    a x p q r s
    0 0 1 1 1 0 <-- inconsistency
    0 1 1 1 1 0 <-- inconsistency
    1 0 0 1 1 1
    1 1 1 1 1 1


We have now disproven the proposition `Optional a -> a`. It is not possible to inhabit this type signature consistently.

Further Reading:



	
  * [The Curry-Howard Correspondence](http://en.wikipedia.org/wiki/Curry%E2%80%93Howard_correspondence)

	
  * [The Curry-Howard Isomorphism with a crash course in formal logic](http://en.wikibooks.org/wiki/Haskell/The_Curry-Howard_isomorphism)


As always, I hope this helps!

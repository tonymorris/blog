---
comments: true
date: 2008-08-15 09:21:40
layout: post
slug: introductory-c-h-and-static-typing
title: Introductory C-H and Static Typing
wordpressid: 211
tags: Programming
---

We use logic every day. No, I don't mean just us programmers; I mean all of us. Here is an observation about propositional logic:


> I have three propositions P, Q and R.

It doesn't matter what these three propositions actually are, it will always hold that, "if P then Q implies that if Q then R implies that if P then R".


I've subverted that a little to try to make it read a little better, but perhaps look it at it a bit more formally:

    
    forall P Q R. (P → Q) → (Q → R) → (P → R)


You might like to try it out on paper using some example propositions, such as:



	
  * P = Today is Tuesday

	
  * Q = I am going swimming

	
  * R = I have hairy armpits


I won't bastardise it with English again ;) However, I will give you a hint. Here is the truth table for → (logical implication):

    
    A | B | A → B
    0 | 0 | 1
    0 | 1 | 1
    1 | 0 | 0
    1 | 1 | 1


Here is where it gets a little interesting. This same logical statement `forall P Q R. (P → Q) → (Q → R) → (P → R)` can also be expressed in most programming languages. Here it is in Java as the `z` method:

    
    interface F<A, B> { B f(A a); }
    static <P, Q, R> F<P, R> z(F<P, Q> f, F<Q, R> g) { ...


It gets more interesting; there is only _one implementation_ of this function if we assume a terminating subset of the language (for Java, no `null` or throwing an exception). If you add in a side-effect such as writing to a file or the network, then you have perverted the function signature. Java allows us to do that, so we also have to assume a subset of the language that disallows this unfortunate anomaly. This notion of a type signature having only one implementation is called _once inhabited_.

Let's write out the truth table for this. Notice the consistent `1` values in the final column. The statement is a tautology.

    
    P | Q | R | P → R | Q → R | P → R | (Q → R) → P → R | (P → Q) → (Q → R) → P → R
    0 | 0 | 0 | 1     | 1     | 1     | 1               | 1
    0 | 0 | 1 | 1     | 1     | 1     | 1               | 1
    0 | 1 | 0 | 1     | 0     | 1     | 1               | 1
    0 | 1 | 1 | 1     | 1     | 1     | 1               | 1
    1 | 0 | 0 | 0     | 1     | 0     | 0               | 1
    1 | 0 | 1 | 0     | 1     | 1     | 1               | 1
    1 | 1 | 0 | 1     | 0     | 0     | 1               | 1
    1 | 1 | 1 | 1     | 1     | 1     | 1               | 1


Under the Curry-Howard Isomorphism, logical implication is represented by a function or in the example above using Java, by the `F` interface. The `z` function is called _function composition_. It takes the two give functions/propositions and composes them.

In Haskell, function composition is not called `z` as we called it with Java, but instead `(.)`. That's a full-stop character in parentheses. We can ask for the type of `(.)` in the GHC interpreter:

    
    Prelude> :type (.)
    (.) :: (b -> c) -> (a -> b) -> a -> c


We might even call it by passing in function instances (similar to instances of the Java `F` interface):

    
    Prelude> ((+1) . (*7)) 20
    141
    
    Prelude> (reverse . map Char.toUpper) "hello there"
    "EREHT OLLEH"


You could do the same using Java, but it's a little more verbose, so I'll omit that ;)

Here is a simple quiz. If we suppose the same `F` interface earlier, how many implementations of this function are there:

    
    
    static <a> F<A, A> t() { ...
    


What about this one?

    
    
    static <A, B> F<A, B> u() { ...
    

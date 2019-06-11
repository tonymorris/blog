---
comments: true
date: 2008-09-05 10:57:44
layout: post
slug: proving-the-existence-of-curry
title: Proving the existence of curry
wordpressid: 285
tags: Programming
---

Prove `∀A. ∀B. ∀C. ((A∧B)→C)→A→B→C` with a truth table.

WTF does that mean? It means that for any three propositions (A, B, C), then the conjunction (logical AND) of A and B implies C implies A implies B implies C. Note that → is right-associative, so X→Y→Z is the same as X→(Y→Z). If you are unfamiliar with denotational semantics (these crazy symbols), then perhaps this more relaxed notation makes sense `forall A B C. ((A, B) -> C) -> A -> B -> C`. Remember this notation, because we will use it later.

Here are some truth tables for conjunction and implication to get started:

    
    
    Conjunction
    P  Q  P∧Q
    0  0  0
    0  1  0
    1  0  0
    1  1  1
    
    Implication
    P  Q  P→Q
    0  0  1
    0  1  1
    1  0  0
    1  1  1
    



We can prove the truth of the above statement by observing a tautology (all true values) in a truth table.


    
    
    1  2  3  4    5        6    7      8
    
    A  B  C  A∧B  (A∧B)→C  B→C  A→B→C  ((A∧B)→C)→A→B→C
    0  0  0  0    1        1    1      1
    0  0  1  0    1        1    1      1
    0  1  0  0    1        0    1      1
    0  1  1  0    1        1    1      1
    1  0  0  0    1        1    1      1
    1  0  1  0    1        1    1      1
    1  1  0  1    0        0    0      1
    1  1  1  1    1        1    1      1
    


Observe that column 8 is true. Therefore `∀A. ∀B. ∀C. ((A∧B)→C)→A→B→C` is a true statement.

Remember this statement earlier `forall A B C. ((A, B) -> C) -> A -> B -> C`? If you have [GHCi ](http://haskell.org/ghc)installed, go to the prompt and type `:set -fglasgow-exts`. Then ask for the type of the `curry` function `:type curry`. You will see this:


    
    
    Prelude> :type curry
    curry :: forall a b c. ((a, b) -> c) -> a -> b -> c
    



Same same! :)

OK, so Haskell is for crazy people who use (oh dear!) logic which has nothing to do with that _real world_ and they should all be writing enterprise Java applications like real programmers, right? Yeah right. Yep uh huh.

So let's write this in Java, just for kicks and just to be noisy. Start with the conjunction and implication primitives:

    
~~~{.Java}
interface Implication<P, Q> {
  Q implies(P p);
}

interface Conjunction<P, Q> {
  P p();
  Q q();
}
~~~



Next write the method for representing the aforementioned statement:

    
~~~{.Java}
class S {
  static <A, B, C> Implication<Implication<Conjunction<A, B>, C>, Implication<A, Implication<B, C>>> s() {
    return null; // todo
  }
}
~~~



I shall leave it as a reader exercise to implement the `s` method. I assure you of only one possible implementation if you do not use `null`, exceptions or side-effects.

Function currying, like all programming concepts, is a logical statement under the Curry-Howard Isomorphism. Have a nice day :)

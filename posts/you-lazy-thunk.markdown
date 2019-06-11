---
comments: true
date: 2007-05-16 14:43:24
layout: post
slug: you-lazy-thunk
title: You Lazy Thunk!
wordpressid: 29
tags: Programming
---

I recently read an article (on C programming) that made the following claim:


> Either f() or g() gets called twice. This is inefficient [where both f and g are equivalent functions.]


Unfortunately, these kind of statements are very common among programmers who generally only use a certain class of programming languages (strict and imperative; C, Java, C# etc.) and I have seen the statement in various forms many times before, but I have been compelled to dispel this myth. I don't intend to pick on this particular article -- after all, the intended topic of the article is unrelated to the fallacious statement, despite being a premise for the article's conclusions and there are many other writers who are most capable of propagating this misinformation anyway. What the author of this statement might not know is that it contradicts some really interesting branches of mathematics including the Lambda Calculus and Complexity Theory (you may have noticed the lambda symbols in the title of this page and you may have seen the term λ-calculus written before).

The space versus computation trade is a standard problem for computer scientists, but recent industry trends have generalised this problem to the point of actually not appearing like a problem at all. You see, space has become cheap, **really** cheap -- so cheap that it almost appears infinite. Let us consider the problem of finding the nth element of an array and a linked list. For the array, this will execute in _constant time_, however, for the linked list, the time to compute it will depend on the value of n. Notice how the array exists in-memory while the linked list might not (e.g. it may read from a file as it is traversed). That is, it has incurred a space cost. This cost has provided the benefit of computation speed. A trade has occurred, not an efficiency gain.

The use of the linked list is called **lazy evaluation** (or laziness) while the use of an array is called **strict evaluation** (or strictness). Traversing the linked list is sometimes called **thunking** (which is just an obscure word for invoking a function) and evaluating each of its elements is called **Weak Head Normal Form (WHNF)**, which is a term taken from the λ-calculus.

Given a lazily evaluated structure, it is possible to make it strict by bringing it to WHNF. This computation will occur once and its result may be placed in a universally-available table with constant time lookup to prevent computing the value again. This storage will incur space and is a process called **memoisation**. Unfortunately, a strict structure cannot be made lazy since the space cost has already been incurred. In fact, doing so yields no benefit whatsoever (other than to appeal to a type system).

We can observe laziness and strictness in some of our _poor-yet-popular_ programming languages like Java, where an array represents a strict, homogenous list, while an InputStream or Iterator represents a lazy list. A ByteArrayInputStream is a (unfortunate) attempt to construct a lazy structure from a strict one with no additional benefit -- in this case, it is to appeal to a type system (e.g. to use a method that must accept an InputStream instead of a byte[]). A Haskell list, which is lazy, is more like a Java iterator than a Java list or array (but still with a significant difference) and it is often better to think this way if you are learning a lazy language coming from a strict, imperative language. In fact, like a Haskell list, a Java iterator can have an infinite length where its `hasNext` method never returns `false`.

We can see now that if _f() or g() gets called twice_, then this is not inefficient, nor is it efficient. This is actually a trade. Computation has been spent while space has been gained, since the result of the computation of the first call of either function is not stored, but is recomputed. Similarly, purchasing a bottle of water for one dollar is not more or less efficient, assuming that the bottle of water is indeed worth one dollar. You are no more or less better off whether you purchased the water or not. Of course, we rarely see such purist forms of capitalism, but the analogy makes sense otherwise :)

Programmers of strict, imperative languages do not always elect to strictly evaluate from what is called a _universe (or domain) of discourse_ (from Set Theory). That is to say, from the universe (U) that is passed to the programmer, each expression is not necessarily brought to WHNF. Ideally, only what is necessary is evaluated to complete the computation and anything else from U is left untouched, however, the decision of what to evaluate is often (at least in my observations) totally arbitrary due to a lack of formal reasoning and inexperience with the foundations of computer programming -- on the part of the programmer. If a subset of the universe is evaluated that is never needed to complete the computation, then this **is** an inefficiency and it happens very often in strict, imperative languages. (Think tip: Is it any wonder your Java application consumes all your volatile memory?)

Let's demonstrate this. Suppose a Java programmer is asked to compute an int -- the sum of the first 2 bytes from an InputStream, or compute 1024 if the bytes are not available. The InputStream represents a potentially infinite universe of discourse (|U| = ∞), however, not all of it is necessarily evaluated (if it were infinite, the function would never terminate). Here is a canonical solution:

    
~~~{.Java}
int firstByte(InputStream in) throws IOException {
  final int i = in.read();
  final int j = in.read();
  return i == -1 || j == -1 ? 1024 : i + j;
}
~~~


Notice how the entire InputStream is not read until the end (if there is one), _simply because you can_, since this would be an inefficiency. The domain or universe is an InputStream, but only two reads occur. Indeed, if this InputStream were infinite, the function would still terminate. Similarly, the following equivalent Haskell function will terminate:

    
~~~{.Haskell}
let first (i:j:_) = i + j; first _ = 1024
~~~


...even if passed an infinite list:

    
    
    > first [20..] -- from 20 to infinity
    41
    


If a byte[] were passed to the `firstByte` function instead and the entire array were never evaluated for the computation (why would it?), we would have an inefficiency. If the array was fully evaluated during computation, then we have neither an efficiency, nor an inefficiency. Certainly, evaluating beyond the universe of discourse yields no benefit whatsoever while incurring a space cost. Unfortunately, this is not immediately obvious because _poor-yet-popular_ programming languages sometimes make it cumbersome to write the desired expression without incurring this inefficiency and so it appears as if it were a necessary trade with the language itself. That is, the reasoning "because this language is cumbersome to such an extreme, it will force you to incur inefficiencies by evaluating outside your domain for computation so as to avoid writing code that is cumbersome" is often overlooked, typically because the alternatives are not known. Also, the more elegant solution, even in that cumbersome programming language, is often overlooked. Nevertheless, I find this behaviour of both the programming language and the programmer using that language, completely absurd (I love you Alex ;))

However, languages such as C/Java often make laziness very easy. Suppose you write an `if/else`. The runtime will not evaluate both the if and else block before executing one or the other. This is lazy behaviour. Similarly, the ternary operator (?:) is also lazy. Finally, the && and || operators are also lazy, since their second argument is only evaluated on-demand. Therefore, we see clearly that laziness is not a completely foreign concept to (mostly :)) strict, imperative languages and is often preferred, since the contrary may certainly result in an inefficiency.

I leave now with the following transcript from my GHC/Haskell interpreter (which is inherently lazy):

    
    
    > take 5 [1..] -- take the first 5 elements from the list of 1 to infinity
    [1,2,3,4,5]
    > foldr (&&) True $ repeat False -- fold the conjunction of True and an infinite list of False
    False
    > foldr (||) False $ repeat True -- fold the disjunction of False and an infinite list of True
    True
    > -- foldr (||) True $ repeat False -- eek! this will never terminate!
    > take 5 $ map (*2) [1..] -- take the first 5 elements from the list with the function (*2) mapped across 1 to infinity
    [2,4,6,8,10]
    

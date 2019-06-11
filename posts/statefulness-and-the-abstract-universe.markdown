---
comments: true
date: 2007-02-22 11:50:01
layout: post
slug: statefulness-and-the-abstract-universe
title: Statefulness and the Abstract Universe
wordpressid: 25
tags: Philosophy, Programming
---

Quite often, I hear comments from people who are trying to get a grasp on functional programming about entities which are "inherently stateful" or "intrinsically mutable" ([example](http://programming.reddit.com/info/14m2o/comments/c14pqp)). The commenters often point to a disk or network to make the point. In this post, I am going to attempt to portray a slightly deeper understanding of this topic to erase the facade and bring to fruition the fact that this "inherent state" (or whatever) is in fact an illusion. This will require some _thought experiment_ and the manipulation of some abstract entities in order to attain this insight.

I will first start off by describing the distinction between the physical universe as we observe it and the abstract universe. The physical universe is made up of matter; for example, a house, a typewriter and even a hard disk. The abstract universe is one that is, well, abstract -- one that is not physical. If we consider the number 2. We call it 'a number', a physical entity, yet we cannot hold it in our hands or point to its physical representation. Therefore, 2 is better described as a representation of "twoness" or "the concept of two". The symbol '2' is used to denote the concept of two, among other possible representations of the same concept:



	
  * two

	
  * . .

	
  * 00000010



...and so on. Anyone interested in defining the concept of two, with a bent for history, might be interested in learning about [the discovery of zero](http://www.google.com.au/search?q=the+discovery+of+zero).

Now, this point touches on some philosophical grounds that I'd rather not go into, so instead, I will make a note of the fact that a pure evolutionary Atheist might not accept this distinction (perhaps claiming that 2 is in fact physical as a chemical signal in the brain), while a Theologian would describe the abstract universe as your 'Soul' (or some such). I don't mean to encroach on this philosophical point and I am hoping I can get away with using terminology that can be translated to the relevant philosophical view.

So, when you write a function, in any language, f(2), you're not passing a 2 or 2 itself to that function, but instead, a representation of the concept of two to the concept of your function and nothing further. Here's the clincher: **All software exists in the abstract universe**. Even your C program that does clever pointer arithmetic and your assembly program that moves the hard disk head. Yes, that FILE* is in fact, an abstraction of a pointer, _not_ a pointer itself. None of these are physical entities, but abstractions of physical entities that manifest themselves somehow -- none of these manifestations are relevant to the software author.

It's all quite simple so far, but if you can pass 'a representation of the concept of two', then what is to stop you from passing 'a representation of the concept of the file system'? or the network? **Nothing, that's what.** In fact, you could quite plausibly argue that this is exactly what [Haskell's IO monad](http://www.haskell.org/ghc/docs/latest/html/libraries/base/Prelude.html#t%3AIO) is doing -- an abstraction that represents these physical entities using the expressive type system of Haskell. In exactly the same way that 2 is an abstraction that represents a physical entity i.e. it will eventually manifest itself as electrical signals in your computer hardware (or physically manifest somehow anyway).

Those of you who are concerned about passing an entire file system as a function argument and the impact on performance might be interested in delving further into a topic called Lazy Evaluation and Weak Head Normal Form (WHNF). I'd rather not reiterate the work of many others who are more dedicated, so I'll just point out that concerns for performance are definitely valid, but the impact on performance is not there (in fact, often quite the contrary -- [performance improves!](http://shootout.alioth.debian.org/gp4/benchmark.php?test=all&lang=all)).

The distinction between '2' and 'the file system' and 'the network' from a software developer's perspective is entirely superficial and should be abandoned. One cannot be 'inherently stateful' while the other isn't. It's one or the other, so which is it? (hint: n**ther :))

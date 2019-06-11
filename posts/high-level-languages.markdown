---
comments: true
date: 2007-07-24 09:15:43
layout: post
slug: high-level-languages
title: High Level Languages
wordpressid: 37
tags: Programming
---

C, Java, C# et. al. (imperative with poor type systems) languages are low level languages. Many people are not aware of this fact, especially those who are only aware of languages that are imperative with poor type systems. This behaviour is probably best described by Psychologists than I will attempt, so I will not attempt any explanations for it.

I refer to pages 58-61 of Types and Programming Languages (0-262-16209-1) on the chapter about the untyped lambda calculus where author Benjamin Pierce notes that the the value false for the Church Boolean encoding is the same as the value zero for the Church Numeral encoding.



> 
λt. λf. f
λs. λz. z




Pierce then goes on to note that:



> 
Similar "puns" are common in assembly languages, where the same pattern of bits may represent many different values -- an int, a float, an address, four characters, etc. -- depending on how it is interpreted, and in **low-level languages such as C** [emphasis mine], which also identifies `0` and `false`.




I wonder if the aforementioned programmers -- those aware of only low-level languages, therefore call languages of "the highest level that I am aware of", high-level languages -- are willing to claim that Pierce has made a grave mistake, or if it will invoke the possibility that maybe there are _more levels_ out there.

If, for just one person, it is the latter, then the objective of this post has been achieved ;)

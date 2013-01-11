---
comments: true
date: 2008-11-30 11:35:51
layout: post
slug: why-functional-programming-matters-in-short-prose
title: Why Functional Programming Matters in short prose
wordpressid: 451
tags: Programming
---

[Why Functional Programming Matters](http://www.md.chalmers.se/~rjmh/Papers/whyfp.html) paraphrased -- a result of a discussion in an IRC channel. Others may find value.

A program may be represented as a function that accepts some input and produces some outputλ This function may be composed of parts; other functions and values.

Suppose any function that exists. I will suppose one that is made of parts A, B, C and D. I might denote this A ∘ B ∘ C ∘ D. Now suppose you desire a function that is made of parts A, B, C and E. Then the effort required for your function should be proportional to the effort required for E alone.

The closer you are to achieving this objective for any function, the closer you are to the essence of (pure) functional programming. This exists independently of the programming language, though programming languages will vary significantly in the degree to which they accommodate this objective.

Imperative programming -- specifically the scope of a destructive update -- is the anti-thesis of this objective.

---
comments: true
date: 2011-03-26 12:29:47
layout: post
slug: a-brief-point-on-static-typing
title: A brief point on static typing
wordpressid: 1002
tags: Programming
---

This post also incidentally attempts to justify my answer to a question that I regularly encounter, _what is your favourite programming language?_



> 
It's not possible to write bug-free programs.




This particular myth is quite popular, and its recent inclusion in a discussion I had has prompted me to write this post. The profound fact of this matter is that this statement is "as false as you can get." In other words, there exists a formal proof of its negation, and subsequent counter-examples. It has also been my experience that belief in this myth has degenerative practical implications.

To emphasise the point, there exist programming languages for which _it is impossible to write an incorrect program_. What does it mean for a program to be correct? It means the program _terminates_. Such languages include (in order of my decreasing experience with them): Coq, Agda, Epigram, Isabelle.

Some people like to think "correctness" includes the thoughts of one or more persons in order to make the assessment. For example, one might proclaim, "sure you have a proof of program termination, but that is not the program that I asked for!" I think this is a poor use of the term "correctness" and I am not considering it any further here.

There is a problem with the aforementioned programming languages. A big one. While you always have correct programs in these languages, _you cannot have all general programs_. This is a well-documented contention. So you might say that these languages set out to achieve the objective of emphasising correctness, but at the expense of generality -- in other words, they are exploring the question: "just how general and practical can we get, without sacrificing absolute correctness?" In my opinion, this is a very important question and worthy of further research.

Other programming languages sacrifice correctness for generality. In my typical work (I work for a product company), this includes languages such as Haskell, Scala and even Scala's type system (which can be used as an embedded language). I expect most of my readers fall into this category of usage of languages. That is, we are all using languages that explore the question: "just how correct and practical can we get, without sacrificing generality?" In my opinion, this is a very important question too.

In the pursuit of answering both of the above questions, there are a number of contentions that arise. This means that there is not a _holy grail_ programming language. It also means that there is a lot to understand before even starting to talk about the merits of correctness (aka static typing). This is unfortunate given the strong compulsion for, and quality of, popular commentary -- we spend too much time clearing up myths. I digress.

However, this issue of contentions and trade-offs does not preclude the existence of questions of dismissal of programming languages. That is, it is possible to ask, and even answer, the question of whether or not there are programming languages that offer nothing toward resolving all of these (meaningful) contentions with respect to peer programming languages. These programming languages may be dismissed as offering nothing toward the goals of computability. It may be said that they are "universally impractical" with respect to the goals of computability (I am explicitly distinguishing here from social objectives). This may seem pessimistic, but it's just a fact of the matter.

I hope that helps.

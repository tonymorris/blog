---
comments: true
date: 2011-01-17 11:37:29
layout: post
slug: critique-of-oderskys-scala-levels
title: Critique of Odersky's Scala levels
wordpressid: 916
tags: Programming
---

Martin Odersky, creator of the Scala programming language, recently wrote [a brief article describing levels of Scala expertise](http://www.scala-lang.org/node/8610). Martin labelled the levels in increasing order: [A1, A2, A3, L1, L2, L3].

I wish to register a small objection here, with recognition of otherwise sound judgement. While the objection is small, I think the consequences of what I believe is an error, are quite detrimental.

I will repost Martin's specification for L3, the highest:



> 
Level L3: Expert library designer


> 
> 

>   * Early initializers
> 

>   * Abstract types
> 

>   * Implicit definitions
> 

>   * Higher-kinded types
> 





First, I will get a couple of nitpicks out of the way. Martin's L2 specifies that variance annotations will be used at this level. I (we: scalaz) propose that there is another level again, where you come to the realisation that variance annotations are not worth their use in the context of Scala's limited type-inferencing abilities and other details. Instead, prefer a short-hand version of `fmap` and `contramap` functions. Scalaz calls these ∘ and ∙. The details of why this is more appropriate are beside the point here. Nevertheless, Edward Kmett has set out to prove this hypothesis wrong. Many of us have and failed. As far as I know, there is nothing compelling yet but I wish Ed luck with his unique insights.

Second, the suggestion that L3 requires the use of early initializers boggles my mind a little. The idea that these details exist at all is a symptom of a lot of nasty language and interoperability issues. Ultimately, if you are relying on the language-specified initialization order in your library code, then you are doing something disastrously wrong. Long-time users of Scala will recall the number of changes this behaviour went through. There is a gremlin at every turn and (much) more disciplined approaches to library design.

To the point.

If L3 is the highest level attainable, where we are using higher-kinded values and implicits, then what about other levels that are _way higher_ in the level of required understanding? Let's be clear, the use of these features has been part of Scalaz for a few years now. I started Scalaz in 2007, when I learned that Scala had these two features (which are extremely important to library design). These features have enabled the encoding of much more advanced concepts -- what about those?

Some concepts that belong way higher than Martin's L3 specification are:





  * encoding type-level transformations


  * advanced purely-functional data structures


  * automated specification-based testing and associated library authoring requirements to effectively achieve it


  * encoding algebraic structures of category theory and _ensuring these are available for practical use_


  * understanding type theory, its goals and (truly understanding, for real) the benefits and trade-offs of static program verification


  * attempting a high (read: extremely high) degree of abstraction in the context of Scala's limited laziness abilities.



I take issue here because keen learners may be fooled into believing that as they are confident in understanding all concepts at L3, then they are qualified to be an "expert library designer." I put it to you that there is an extraordinary leap from L3 to an API designer of any considerable merit. Indeed, in my opinion, L3 is just enough knowledge to even begin practical library design. Before I attract a charge of shooting too high or the usual anti-intellectual nonsense, I like to remind others that I like to aim high, very high. I encourage you to do so too. Nothing more.

I recently gave [a beginner API design quiz](http://blog.tmorris.net/scala-exercise-with-types-and-abstraction/). I use it in programming classes that I teach, where many people struggle with it. When these students eventually break through, they are equipped with a reasonable fundamental understanding of writing practical libraries and APIs. However, note the triviality of the problem and the observation (if you'll trust me), that many programmers have difficulty. This leaves open a huge opportunity for improvement in problem-solving skills -- let's take it!

I'd hate to believe that L3 is "expert." I'd be saddened to think others have been tricked into settling there too. Surely we can shoot higher than that Martin.



## Edit



A case in point, I was directed to [this comment](http://www.reddit.com/r/programming/comments/f38z4/scala_levels_beginner_to_expert_application/c1cyp2j):



> 
If it [scala] is a failure, why has the language - for exaple [sic] - the best collection library of all languages currently out there?




Honestly and without any intention to exaggerate, it boggles my mind with fascination as to what could cause such a rapid and extreme departure from reality. Working with both Scala and Haskell in depth for many years, there is nothing more disheartening than putting Haskell aside and dealing with the underwhelming practicalities of Scala's (and therefore, Java's) libraries. Indeed, this Great Big Hole That Is Still Gaping Wide was one of the primary motivations for the creation of Scalaz when I was working on a commercial application yearning for useful libraries. That was many years ago. Nothing has changed.

That's just Haskell. There are other examples of programming environments that completely annihilate Scala/Java in terms of useful libraries.

To believe the aforementioned statement is perhaps a symptom of shooting so disastrously low as purported by "L3, the highest level of expertise." In other words, there are serious and observable consequences for having such low standards, to such an extent that one may even fall under the illusion presented in this statement. I hope you'll agree that there are practical implications for believing these things.

Then I witnessed [this comment](http://www.reddit.com/r/programming/comments/f38z4/scala_levels_beginner_to_expert_application/c1czlg9), which completely misses the point. Specifically, this commenter is not at what Odersky would call L3, since the comment demonstrates lack of understanding of higher-order polymorphism (aka higher-kinds). I don't wish to pull apart every naive comment, but I do wish to point out that we can aim much higher than this. 

**MUCH MUCH HIGHER**

I lament, but fight on, for the encouragement of setting a higher standard. Sorry guys, but you're way off base. We can do a fuck-load better than this and we have done exactly that, elsewhere.

_-- a former contributor to the Scala collection libraries with high hopes_

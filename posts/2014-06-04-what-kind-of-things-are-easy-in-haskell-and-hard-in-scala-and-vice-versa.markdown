---
comments: true
date: 2014-06-04 00:00:00
layout: post
slug: what-kind-of-things-are-easy-in-haskell-and-hard-in-scala-and-vice-versa
title: What kind of things are easy in Haskell and hard in Scala, and vice-versa?
tags: Scala, Haskell, Programming
---

*On 24 February 2011, Daniel C. Sobral writes:*

There has been some intermingling of Scala and Haskell communities, and I have noticed now and then people commenting on stuff that's supposed to be easy in Haskell and hard and Scala. Less often (maybe because I read Scala questions, not Haskell ones), I see someone mentioning that something in Scala is easier than in Haskell.

So. I'd like to know from people who are knowledgeable in both what kind of things are easy in Haskell and hard in Scala, and, conversely, what kind of things are easy in Scala and hard in Haskell.

----

*Tony Morris responds 9 hours later:*

Daniel, As you know my day job is primarily writing Haskell and secondarily Scala. I have also used both languages for teaching, though not in universities (I use other languages there), but mostly for voluntary teaching that I still do today. Very rarely, I use Java or Javascript. I work for a product company.

I am pleased to see that my prediction is false -- your question has not provoked a slew of misinformation as I thought it would. As a result, I am compelled not to ignore it :) So here goes.

At a somewhat superficial level, Haskell has significantly superior tool support over Scala and Java. For non-exhaustive example, Haskell has hoogle, djinn and pl; three tools that alone are extremely useful for which there is no equivalent for Scala. These tools exist and are as useful as they are, because of certain fundamental properties of Haskell itself. Here I mean, the hoogle function is only as useful as it is because Haskell tags IO effects in the type delineating values with types IO t and t, so hoogling for say, [a] -> Int eliminates a lot of candidate functions that would have this type in other environments. In Scala, without the delineation between an Int that has been computed with its arguments, and an Int that has been computed with the entire universe, a hoogle-equivalent would not be as useful -- nevertheless, it would be somewhat useful were it to exist.

Haskell's hlint is also superior to say, Java's CheckStyle. Included in GHC is a warning system, which when coupled with hlint is far more comprehensive. I've not seen anything like this for Scala.

Haskell has cleaner syntax and superior type inference. Very rarely is it the case that we must type-annotate our Haskell. This is not to say that we do not type-annotate our Haskell, just that we have the choice. As you know, this is not so with Scala. However, on an extremely superficial level, I prefer Scala's lambda syntax to Haskell's, which requires a pseudo-lambda symbol (\). In any case, I aim for point-free style where appropriate, making this already-weak point moot. I use Haskell's clean syntax to appeal to children in the challenges of teaching. Children take very easily to Haskell's syntax, since there is far less redundant, "off to the side", "let's take a little excursion to nowhere", ceremony (so to speak). As you might guess, children are easily distracted -- Haskell helps me as a teacher to keep this in check.

On to the fundamentals. As you know, Haskell is call-by-need by default, where Scala is the other way around. This is, of course, a very contentious issue. Nevertheless, I admit to falling very strongly to one side: call-by-need by default is the most practical and Scala's evaluation model is a very far-away second place. I have never seen a legitimate argument that comes close to swaying my position on this, though I do invite it (not here please). To summarise, I think Haskell has a superior evaluation model. There are also nuances in Scala's laziness. For example, while Scala unifies lazy values, it does not do so for those in contravariant position. i.e. a (Int => Int) is not a ((=> Int) => Int).

This contentious issue aside, Haskell's effect-tracking (by the way, which is a consequence of its evaluation model), is overwhelmingly practical to every-day software tasks. The absence of same or similar is quite devastating as many users of Scala have become aware (by wishing for a solution). I cannot emphasise how important this form of static type-check is to typical business programming in terse form, so I won't try here.

Haskell has far superior library support than Scala and Java. You and I have discussed this before. Scala lacks some of the most fundamental functions in its standard libraries and the higher-level ones are even more scarce. For example, there are fundamental properties of Scala making good library support more difficult (strict evaluation, Java interop), however, neither of these incur such a penalty as to produce what can only be described as an unfortunate catastrophe as the Scala standard libraries. That is to say, the Scala standard libraries could easily be miles ahead of where they are today, but they are not and you (I) are left with ponderances as to why -- something about third lumbar vertebrae and expert levels or something I suppose.

To help emphasise this point, there are times in my day job when I come off a project from using Haskell to Scala. This comes with some consequences that I feel immediately with a very sharp contrast; I then use intellij idea which is slow, cumbersome and full of bugs, the Scala type inferencer is less robust and difficult to prototype with (I may make a type-error and it all just carries on as if nothing happened), but there is nothing more disappointing than having to spend (waste) a few hours implementing a library that really should already be there -- what a waste of my time and probably the next guy who has to implement such fundamental library abilities. Talk about shaving yaks. In my opinion, this is the most disappointing part of Scala, especially since there is nothing stopping it from having a useful library besides skill and absence of ability to recognise what a useful library even is. This happens for Haskell too, but to a far lesser extent. I digress in frustration.

The GHC REPL (GHCi) has better support for every-day programming. More useful tab-completion, :type, :info and :browse commands are heavily missed when in Scala. It's also much faster, but Scala can be forgiven given the JVM.

Why use Scala? I can call the Java APIs, even the most ridiculous, yet popular, APIs ever written. I can call WebSphere methods and I can write servlets. I can write a Wicket application, or use the Play framework or I can do something outragoeus with a database and hibernate. I can do all those things that our industry seems to think are useful, though I secretly contend are pathetic, and I hope our children do too. I can completely avoid the arguments and discussions associated with the merits of these issues, and get on with the job, while still getting the benefits of a language that is vastly superior to Java. This might seem like a cheap stab, though I am sincere when I say that is a true benefit that I value a lot.

Scala's module system is superior to Haskell's, almost. That is, it has some things that are very useful that Haskell does not, but also vice versa -- for example, Haskell allows a module to export other modules. Scala requires you to stick your finger in your left ear to do this; oh and intellij idea stops working -- I exaggerate, but you get the point. Scala has package objects and and first-class module niceties. Haskell lacks here.

Scala also has the ability to namespace a function by giving special status to one of its arguments (some people call this OO, then don't, in the very next breath -- I never get it). What I mean is, you may have two functions with the same name, which are disambiguated at the call site by the type of the argument to the left of the dot. I am deliberately not calling this by any special name, but rather focussing on its utility -- Haskell can do this with qualified imports -- not quite so nice. I am usually, though not always, particularly unimaginative when it comes to inventing function names -- allowing me to reuse one without a penalty is very handy indeed. Note here I do not mean overloading -- I think the Scala community has worked out that overloading is just not worth it -- do not do it, ever.

In my experiences, Scala appeals to existing programmers, who can usually get up and running quicker than with Haskell. In contrast, non-programmers get up and running with Haskell far quicker than with Scala. As a teacher, I used to think this was a great attribute of Scala, then I tried it, then I thought it was just good. Today, I think it is just not worth it -- I have seen too many catastrophes when programmers who are familiar with degenerate problem-solving techniques (ala Java) are handed such things as Scala. Call me pessimistic or some-such, but I wish to remind you that a few years ago, I was handed an already-failed project written in Scala by the Java guys, which I was supposed to "save" because I was the "Scala expert." I'm sure you can guess how that turned out. I have many (many) anecdotes like this, though most of those are confirmed each time I try to use Scala for teaching existing programmers, rather than in industry (perhaps this is my selection bias, given my extreme apprehension). Nevertheless, my experiences aside, you may call this a benefit over Haskell -- there is no doubting that existing programmers get up and running much quicker with Scala.

I can think of a few other tid-bits, but hopefully this satisfies your curiosity. I don't know how many people are in my position of using both languages extensively in a commercial environment, but I'd truly love to hear from someone who does -- especially if they take strong objection to my opinions. That is to say, I invite (and truly yearn for) well-informed peer review of these opinions.

Hope this helps.

----

*Recovered from [StackPrinter](http://www.stackprinter.com/export?service=programmers.stackexchange&question=51245&printer=false&linktohome=true) after deletion and much subsequent searching.*

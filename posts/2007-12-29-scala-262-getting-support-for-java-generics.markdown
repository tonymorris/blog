---
comments: true
date: 2007-12-29 08:46:16
layout: post
slug: scala-262-getting-support-for-java-generics
title: Scala 2.6.2 getting support for Java generics
wordpressid: 53
tags: Programming
---

Scala is a far superior language to Java. In fact, of all the languages that I have investigated, it is the most powerful language that also targets the JVM and Java libraries easily (sorry CAL). I have long held the position that _there are no (with one corner case) rational justifications for using the Java programming language for anything but educating our children on how not to design a programming language even when targetting legacy code_. This has of course, been met with 'what about this?' and 'what about that?'[1] all of which I have been able to refute, except for one niggling corner case.

Client code of existing Java-compiled code would not receive the benefits of generics, since they were erased. For example, if you were to call a Java-compiled method that returned a `List<Integer>` from Scala, you'd have a reference to `java.util.List` without the type argument. If you were to call this method from Java, then many methods on the returned `List` would return `Integer` in place of the type argument, however, for Scala, you'd have a type `AnyRef` (an alias for `java.lang.Object`) just like Java did in the pre-1.5 days. This gave Java one (and only one) case where its use was an improvement over the use of Scala.

This has been the only remaining use case where Scala does not match or exceed Java in ability, however, Martin Odersky (creator of Scala) has revealed that the next version of Scala is to do away with this limitation! Furthermore, we can use the feature today! That is, we can start moving over any Scala <= 2.6.1 code to 2.6.2 right now and have the full advantages of a powerful language and do away with Java forever, unless of course, you're still out there appeasing the irrationalities of the suits (you poor buggers, no really, I mean it).

From Martin on the Scala mailing list:


> 
Just to confirm:

The nightly build of Scala contains now support for Java generics. The
first official release supporting this will be 2.6.2. For instance
java.util.List is considered a generic type in Scala in this release.
I announce now so that you can experiment with the new feature. Maybe
you also already want to start to migrate your codebase to the new
scheme.

More precisely, the following things have changed:
_continues_




[1]
Here are some that I have met and convincingly (and often quite easily) debunked in conversation. Perhaps I could write about them some time.



  
  * From the CTO of X Corporation, "I don't have any programmers capable of using Scala". This myth is both easily falsified, but also extremely detrimental to the interests of the organisation -- far more than is initially realised. That is to say, after debunking this myth, my opponent in the argument often has an enlightening experience and then an "Oh no! Given this (new understanding), look at what (detrimental decisions) I have been doing to the organisation!". Of all fallacies, this is the one I'd like to put in the box of past events from which to learn.

  
  * 
     But Java is enterprise-ready
  

   
  * 
     But Java is a robust language with many libraries
  



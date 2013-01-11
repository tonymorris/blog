---
comments: true
date: 2008-10-16 06:44:54
layout: post
slug: javaruby-does-not-generalise-to-staticdynamic
title: Java/Ruby does not generalise to static/dynamic
wordpressid: 384
tags: Programming
---

So you were once a Java fanboy and now you've apostatised to the Ruby cult. Now you believe you hold veritable opinions about "statically typed languages" and "dynamically typed languages". How wrong you are.

Java is not representative of static type systems, not even a bit. It lacks some of the most basic features of a static type system. It might even be said that its static type system imposes some of the most impractical insanity that could possibly be conceived. Why do I think this? [Because I have pushed Java's static type system to its limits](http://functionaljava.org/) and relative to its peers, it fails miserably. I've seen the language inside-out. I used to work on the JDK implementation for a certain corporation (of questionable ethical standards). I even have the spanky Sun Java Programmer/Developer certifications. Yes I have been drowned in Java Jolly Junkie bullshit and revived.

So you use Ruby eh? It allows you to express things that Java was too rigid to allow, right? That doesn't mean a thing about static type systems, right? RIGHT?

When you use Java and Ruby as the example for static and dynamic type systems, I am reminded of my children who compare the flight of their scrunched up pieces of paper... er highly aerodynamic, radar-resistant aircraft (sorry boys). Hopefully some day they will learn what a wind tunnel is and what it takes to engineer a fighter-aircraft. Will you?

This kind of enthusiasm is to be encouraged -- if you are 6 years old -- but you are not, so you just look like you are in an adult's body. No really, you do. This might be fine with you (who am I to judge?), but if it is not, it might help to know [What to Know Before Debating Type Systems](http://www.pphsg.org/cdsmith/types.html). Just a thought -- there is plenty of other information available should you desire to seek it, just drop me a line.

Here are some tips to get started; static typing and explicit type annotating are two very different things. Don't use the two notions interchangeably -- not even loosely. Dynamically-typed languages do not produce shorter code, this is a myth. In fact, what constitutes shorter code is often misunderstood -- certainly by Ruby (and Python, Groovy for that matter) advocates that I have encountered. I'm tempted to dispense with this myth with a challenge, but I'm not sure if I can be bothered (I have a life outside of whinging on my blog you know). It would also fail to do the topic justice. An entire discussion on what constitutes shorter code is in order.

For the love of GADTs, in the meantime, please stop talking nonsense about static and dynamic type systems. It only serves to hurt other potential learners by perpetuating misinformation. Myths spread by repetition -- be honest and break the cycle.

---
comments: true
date: 2010-09-29 21:13:04
layout: post
slug: reversejar
title: reverse.jar
wordpressid: 815
tags: Programming
---

There was once a programming language called Jolly. It was exactly like what would become Java, except for a couple of differences.



	
  1. The language did not have parametric polymorphism of any kind. No generics, nadda. In fact, the inventors of Jolly had never even heard of it.

	
  2. The language did not have a common supertype. No `java.lang.Object`


The Jolly language was incredibly popular, especially among enterprise programmers. There were massive open source contributions written in Jolly, including one library, which was called **list.reverse**.

The **list.reverse** library had about 140 developers (and getting stronger!) across the world all working away. You see, since Jolly had no parametric polymorphism, every time you wanted to reverse a list of say, `Banana`s, you had to check if reverse was already written for Bananas in the library, or if not, write it and commit to HEAD.

An academic, Robin, was chuffing on a joint rolled in some bark on which he had scribbled the thesis for the pi-calculus, after having invented an advanced ML programming language (with generics), spent a few years launching a space probe with it, found the challenge uninteresting, then decided to take on the harder stuff, made the effort to let the Jolly developers in on a little secret, "Hey guys, parametric polymorphism -- it was invented decades ago, has been formalised and shown to be useful in many domains, including this one."

"Who _was_ this guy!? How dare he interrupt us on our pursuit to solve real-world list reversing with this academic nonsense!" was the first post to the mailing list. The Jolly contributors were _furious_ at the careless remark of this Robin guy who, as far as they were concerned, had never written a line of industry-grade software in his life.

"Guys, just saying. You don't need to keep writing this library over and over. I thought you might like to know. Just introduce this language feature and you're done. I've attached a patch."

Well now didn't this get under the nose of the Jolly contributors. All that hard work had just been undermined in a short email by this person who had never written a single line of Jolly in his entire life. What would he know about list reversing anyway!? We have years of experience!

The academic finished off his joint and visited his colleague down the hall, Hermann, who was about to publish a thesis on new findings in the Lie Algebra, "those guys really got upset eh?", Hermann remarked. Robin, not giving much of a shit, made a noise through his nose then asked, "So what's new?"

It's now a few years since that email. The patch to Jolly was never looked at. The list.reverse developers recently released a version 5.0 of their library, which implements reverse for lists of every single class in the Jolly library, and includes a plug-in for every type in the most popular ORM library for Jolly. The project was also forked to include reverse for both arrays and lists and is now more popular than list.reverse.

Robin and Hermann have since forgotten that Jolly even exists. Their colleagues have advanced computational theory well beyond what will soon become Java.


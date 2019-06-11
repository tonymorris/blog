---
comments: true
date: 2006-12-19 13:59:41
layout: post
slug: fix-it-sun
title: Fix it Sun!
wordpressid: 17
tags: Programming
---

This is a call out to renew pressure for Sun to fix the [tail call elimination defect/RFE](http://bugs.sun.com/bugdatabase/view_bug.do?bug_id=4726340) that has existed in HotSpot since inception and still exists today. The IBM JIT is very much capable of tail call elimination, as is the .NET CLR (it even has an instruction built-in!) but still Sun's weak compiler dominates our industry and stifles further innovation in software development.

It has long been known that imperative programming has some inherent weaknesses and [people](http://scala.epfl.ch/docu/files/ScalaByExample.pdf) [all](http://docs.msdnaa.net/ark_new/Webfiles/WhitePapers/Babel01/bab12.pdf) [around](http://www.artima.com/weblogs/viewpost.jsp?thread=176597) [the](http://citeseer.ist.psu.edu/schinz01tail.html) [place](http://www.delorie.com/gnu/docs/kawa/scm2java.html), who recognise this weakness, are trying to remedy it, only to be stopped in their tracks by the fact that this industry continues to use a very poorly implemented compiler.

Today, fixing this defect is not in Sun's interest (nor that of IBM). In fact, doing so may well speed up the death of its programming language by many orders of magnitude. I postulate that as [Sun squeezes its last few breaths out of Java](http://gafter.blogspot.com/2006/08/closures-for-java.html) and as more and more people enlighten themselves with much more powerful alternatives, Sun will inevtiably concede to this pressure. I declare that I have an agenda to speed up this process. Therefore, I would like to apply pressure to Sun to fix this defect so that thousands of existing software clients across the world have a smoother migration path into a more innovative and ultimately, more productive, software development environment. After all, corporations are not necessarily all about stifling innovation, [are they](http://www.google.com/corporate/tenthings.html)?

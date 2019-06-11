---
comments: true
date: 2008-11-07 09:42:55
layout: post
slug: intellij-idea-80-scala-turns-the-tide
title: IntelliJ IDEA 8.0 + Scala turns the tide
wordpressid: 414
tags: Programming
---

I have a couple of whining rants on here about the ineptitude of the IntelliJ IDEA Early Access Program releases and the Scala plugin. It was, at that time, quite unusable, which is fine on its own, but I was concerned by the fact that the number and magnitude of the workarounds was increasing over time.

I am pleased to report that my preliminary use of the recently released IntelliJ IDEA 8.0 and the Scala plugin appears to require significantly less workarounds to the degree that it is now usable. In fact, it is more usable than it has been in a long time.

Myself and at least one other person were initially tricked into believing that there was no Scala plugin available because it was not listed in the Available plugins. There was however, a "Scala Application runner" plugin available. Installing this also installs the Scala plugin.

Unless I am making a judgment too early, I consider IntelliJ IDEA to have redeemed itself from my earlier complaints and I am thankful to the JetBrains team for making it so (with little/no effort on my part).

Interestingly, I note that JetBrains are considering adding automated specification testing and make a reference to ScalaCheck in [this discussion](http://plugins.intellij.net/plugin/?id=1347). I wonder if they are aware of [this alternative (Functional Java)](http://functionaljava.org/) that does the same with a more comprehensive library and also able to be used from standard Java.

Thanks for listening to my whining earlier and thanks to JetBrains for making smooth development with Scala possible :)

Edit: Except this same old bug is still there (it has been there for months) when expanding some source trees (I know I'm not the only one to experience this):

    
    
    Error during dispatching of java.awt.event.MouseEvent[MOUSE_PRESSED,(118,350),absolute(114,375),button=1,modifiers=Button1,extModifiers=Button1,clickCount=1] on frame0
    java.lang.StackOverflowError
    	at com.intellij.psi.impl.CachedValueImpl.a(CachedValueImpl.java:1)
    	at com.intellij.psi.impl.CachedValueImpl.a(CachedValueImpl.java:70)
    	at com.intellij.psi.impl.CachedValueImpl.a(CachedValueImpl.java:3)
    	at com.intellij.psi.impl.CachedValueImpl.a(CachedValueImpl.java:104)
    	at com.intellij.psi.impl.CachedValueImpl.getValue(CachedValueImpl.java:41)
    

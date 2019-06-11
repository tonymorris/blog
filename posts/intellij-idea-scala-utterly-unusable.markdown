---
comments: true
date: 2008-09-24 09:34:48
layout: post
slug: intellij-idea-scala-utterly-unusable
title: IntelliJ IDEA + Scala utterly unusable
wordpressid: 356
tags: Programming
---

It seems that IntelliJ IDEA 8.0 EAP and its Scala plugin have reached the point of being completely unusable. This is such a shame given that there are no existing viable alternatives (don't say Eclipse and expect me to keep a straight face). I guess it will be emacs or Kate in the future.

The last few EAP releases crash the IDE when you expand some source trees resulting in a perpetual wait icon. I receive a dialog about submitting the bug and I have done, three times. The last two EAP releases (8823 and 8810) don't allow me to install the Scala plugin at all. Upon restart, the IDE is unaware that the Scala plugin has even been installed. As a result, I have to use 8664, which my trial is due to expire any time soon.

The IDE constantly gives me false errors (surely it is not difficult to download the Scalaz source and observe the IDE complaining about errors that do not exist?). The IDE causes a segmentation fault if I use the Sun JDK 1.6.0_07 and I receive an UnspportedClassVersionError if I use version 1.5, so I am forced to use 1.6.0_10. The IDE is barely responsive on a 3.2GHz machine with 1.5GB memory and it doesn't even align scaladoc comments properly as you write them. I am constantly manually fixing up indentation in the source code.

With so many elementary failures, I think any reasonable person would concede that IntelliJ IDEA is as unusable as Eclipse. That ends my whinge; thanks for listening.

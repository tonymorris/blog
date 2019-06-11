---
comments: true
date: 2008-01-15 15:16:14
layout: post
slug: scalalistfoldleft-for-java-programmers
title: scala.List.foldLeft for Java programmers
wordpressid: 56
tags: Programming
---

I recently ran another Scala training course and I produced a hand-out that seemed to help people understand what a foldLeft is. Here is a brief explanation of it.

Suppose you have the following Java code:

    
~~~{.Scala}
B b = start

for(final A a : listOfA) {
  b = method(b, a);
}

return b;
~~~


You can have any value for `start`, `method` and `listOfA`. Also, `method` may ignore any of its arguments or even not have them passed at all. You'll notice that a **lot** of Java code is written this way, so it would be fair to say that "left folds occur a lot in Java", despite being encoded as loops.

The above Java code is equivalent to the following Scala code, again for all values of the given identifiers (so long as they type check of course):

    
    
    listOfA.foldLeft(start)(method)
    



...which is also equivalent to:

    
    
    (start /: listOfA)(method)
    



...which by the way, is a perfectly sound approach to performing a list reduction, despite what some clowns rubbish on with.

That's all, hope it helped :)

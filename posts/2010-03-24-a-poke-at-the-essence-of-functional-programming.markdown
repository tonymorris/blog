---
comments: true
date: 2010-03-24 13:04:53
layout: post
slug: a-poke-at-the-essence-of-functional-programming
title: A poke at the essence of functional programming
wordpressid: 709
tags: Programming
---

I used to work for IBM on the Java implementation. I learned the language inside-out. I did all those Sun certifications and other spanky things. I wanted to understand what a lot of people alleged was so special. I didn't ever find it.

I have found that few people can tell me the answer to this question. If you don't know the answer, don't fret; it's not the important part.


    
~~~{.Java}
method(s1.charAt(i), s2.charAt(j));
~~~





> 
Assuming `s1` and `s2` are both of the type `java.lang.String`, then which call to `charAt` will occur first?




Many people would correctly guess at the left-most one. This is correct and is mandated by the specification.

However, on introspection, the reason nobody really knows _is because it doesn't matter_. If the specification implementation had a bug and executed the right-most call first, then we, the programmer, would never even know. We gloss over this every day when we read Java code.

However, does this hold for all functions? What if it was something other than `charAt`? No, unfortunately, this only holds true for a specific set of functions. The `charAt` function is _referentially transparent_. For any given `String` and any given `int` then `charAt` will consistently return the same `char`. This is true for other referentially transparent functions too, but not any arbitrary function. Imagine if `charAt` did a database call or something like that!

Let us suppose now a new language feature that enforced the referential transparency of our functions. If it is referentially transparent, the compiler makes sure of it. Now imagine a new language where **every** function was referentially transparent.

All of a sudden, in the blink of an imagined hypothetical, the explicit order of invocation is no longer important. Just like that, poof, gone.

Welcome to the beginning of an understanding of the essence of functional programming.

Have fun! :)

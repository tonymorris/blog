---
comments: true
date: 2007-08-07 15:02:40
layout: post
slug: imperative-programming-is-a-special-type-of-functional-programming
title: Imperative programming is a special type of functional programming
wordpressid: 41
tags: Programming
---

There are a lot of things that, upon initial investigation, turn out to be false after some deeper analysis. There are a striking number of these propositions that not not only turn out to be false, but it is the inverse that turns out to be true!

One of these is many common descriptions of pure functional programming against imperative programming. Here are some that I have seen:



  
  * 
    functional programming _is on the other side of the fence of_ (or some such) imperative programming. That is, there are two (or more) very distinct, non-unifiable methods or disciplines of programming.
  

  
  * 
    Functional programming is more restrictive than imperative programming.
  



I hope to address the first point another time and on the second point, for now, I will assert that:


> 
Not only is functional programming not more restrictive than imperative programming, but quite the opposite is true! Imperative programming is one specific type of functional programming that is relatively seldom worth using.

In the relatively rare (though inevitable) case of performing I/O, only _then_ will I resort to the imperative programming technique within pure functional programming. I will reason about my imperative code as if it were pure anyway, but I also understand that there are many destructive updates that must be appealed to -- for example, the many common file system types that are used.




I don't wish to elaborate on this statement right now, since I am still deciding how I will do it and I also know how this fact can fail to be recognised by many so I want to get it right. Importantly, it sets (one of) a premise for another issue that I'd like to write about some time.

In the meantime, I want to refer to [Part 2 of Simon Peyton-Jones', "A Taste of Haskell" ](http://blip.tv/file/325646)where he says at 1:06:42:


> 
[Referring to the IO data type] This is imperative programming, embedded within Haskell [pure functional programming].




There it is, right there, in one sentence ;)

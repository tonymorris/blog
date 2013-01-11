---
comments: true
date: 2008-03-11 13:27:23
layout: post
slug: rafs-problem
title: Raf’s Problem
wordpressid: 67
tags: Programming
---


[Raf](http://rafs-blog.blogspot.com/) -- a colleague -- gave me the following problem:



> 
Given a String (x) containing only characters a-z, write a function (f) that returns a base 10 integer, which converts the String as if it were a base 26 numeral. Function f is bijective.




Here are some example runs:


    
    
    x      | f(x)
    empty  | 0
    a      | 1
    b      | 2
    z      | 26
    aa     | 27
    az     | 52
    ba     | 53
    bz     | 78
    aaa    | 703
    aaz    | 728
    aza    | 1353
    



Using your preferred programming language, implement function f.

My solution is in a comment on this page. What is yours?



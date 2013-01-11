---
comments: true
date: 2008-02-01 16:03:24
layout: post
slug: its-just-logic
title: It’s just logic
wordpressid: 60
tags: Programming
---




    
    
    *Don't do this*
    **Do this**
    





* * *



**conjunction (∧)**

    
    
*if(p) q else false*

**p && q**

p ∧ q
    



**conditional/implication (→)**

    
    
*if(p) q else true*
**!p || q**
¬p ∨ q
    



**negation of conditional/implication**

    
    
*if(p) !q else false*

**p && !q**

p ∧ ¬q
    



**negation of conjunction**

    
    
*if(p) !q else true*

**!p || !q**

¬p ∨ ¬q
    



**disjunction (∨)**

    
    
*if(p) true else q*
**p || q**
p ∨ q
    



**negation of inverse conditional/implication**

    
    
*if(p) false else q*
**!p && q**
¬p ∧ q
    



**inverse conditional/implication**

    

*if(p) true else !q*
**p || !q**
p ∨ ¬q
    



**negation of disjunction**

    
    
*if(p) false else !q*
**!p && !q**
¬p ∧ ¬q
    








    
    
*if(p) true else false*
**p**
    




    
    
*if(p) false else true*
*!p*
    

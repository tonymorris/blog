---
comments: true
date: 2008-05-27 10:40:39
layout: post
slug: reductio-testing-equalshashcode
title: 'Reductio: Testing equals/hashCode'
wordpressid: 82
tags: Programming
---

[So you wanna know how to test Java's equals/hashCode huh?](https://issues.workingmouse.com/view.php?id=30)


Here is a true statement that should hold for any `equals` and `hashCode` implementation:


> 
forall x. forall y. x.equals(y) => x.hashCode() == y.hashCode()



_You can read => as **implies**_. So if x equals y, then this implies that the hash code of x is equal to the hash code of y. This is called _logical implication_ and is often written in English as "if p then q", for some p and q, just like I did in the previous statement. Easy peasey eh?

How might we test the `equals` and `hashCode` implementation of `MyClass`? If we were using JUnit (or some other non-automated test tool):

    
    
    assertEqual(new MyClass(7, "abc").hashCode(), new MyClass(7, "abc").hashCode());
    assertEqual(new MyClass(42, "CBA").hashCode(), new MyClass(42, "CBA").hashCode());
    ... etc. etc.
    



What we would **really** like to say is something like:



> 
I assert that for any two equal instances of `MyClass` (call them x and y), then the hashCode of x is equal to the hashCode of y. Dear test tool (Reductio!), please try your best to show me why this is false if you find it to be so.



Well, that's just an English way of saying the logical implication statement above, right? We could write this statement in Java (boiler-plate omitted):

    
    
    Property p = property(arbMyClass, arbMyClass, { MyClass m1, MyClass m2 =>
          bool(m1.equals(m2)).implies(m1.hashCode() == m2.hashCode()) })
    



[Read on...](http://wiki.workingmouse.com/index.php/Reductio_EqualsHashCode)

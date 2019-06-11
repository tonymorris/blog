---
comments: true
date: 2008-05-24 21:00:03
layout: post
slug: reductio-testing-for-the-top-java-programmers
title: 'Reductio: Testing for the Top Java Programmers'
wordpressid: 81
tags: Programming
---

[Reductio](http://reductiotest.org/) is open source (BSD) software that provides some very clever ideas adapted from QuickCheck. In particular, Reductio allows you to write algebraic properties about your software and the software then attempts to falsify these properties using automation of function domain value generation. Reductio can run 1000 unit tests in 4 or 5 lines of code, and that is Java code! (No, I am not kidding; really take a look at some of the [code examples](http://reductiotest.org/examples))

Reductio also provides shrinking of counter-examples in the event of proving a property false; for example, `a + b == a - b` is not always going to be true (for all values of 'a' and 'b'), but would you like Reductio to provide the counter-example `a = 1934, b = 4571`? I think you'd prefer a smaller counter-example such as `a = 1, b = 1` right? I would too :) By providing the smallest reasonable counter-example, Reductio makes it easier to debug your code in the event of finding fault.

Another extremely useful feature of Reductio is _automated mocking_. That is to say, Reductio can generate implementations of an interface for you so long as you provide the mapping from one or more transformations (this exploits a programming pattern called a Functor) to your type. Doing so is guaranteed to be a straight-forward task for the user (so straight-forward that you could automate it with reflection; an add-on feature perhaps?). This allows you to quantify across interface types. For example, you might state a property about your software using some interface, but you assert that the property holds regardless of the implementation of the interface. Reductio will generate hundreds of implementations if you like! All automated of course ;)

Reductio makes it trivial to write generators for user-defined types (again, another potentially automated task that requires Java's meta-programming facility ala reflection), however, Reductio also provides many existing implementations. Following is a _non-exhaustive_ list.

If you're into robustness in the verification of the correctness of your Java software, take a look at Reductio.
[http://reductiotest.org/](http://reductiotest.org/)

---------------------------------------------------------------------------------------------------------



##### java.lang






  * Boolean


  * Byte


  * Character


  * Double


  * Float	


  * Integer	


  * Long	


  * Short	


  * String	


  * StringBuffer	


  * StringBuilder	





##### java.util






  * ArrayList	


  * BitSet	


  * Calendar	


  * Date	


  * EnumMap	


  * EnumSet	


  * GregorianCalendar	


  * HashMap	


  * HashSet	


  * Hashtable	


  * IdentityHashMap


  * LinkedHashMap	


  * LinkedHashSet	


  * LinkedList	


  * PriorityQueue


  * Properties	


  * Stack	


  * TreeMap


  * TreeSet	


  * Vector	


  * WeakHashMap





##### java.util.concurrency






  * ArrayBlockingQueue	


  * ConcurrentHashMap	


  * ConcurrentLinkedQueue


  * CopyOnWriteArrayList	


  * CopyOnWriteArraySet	


  * DelayQueue	


  * LinkedBlockingQueue


  * PriorityQueue	


  * SynchronousQueue	





##### java.sql






  * Date


  * Time


  * Timestamp





##### java.math






  * BigDecimal


  * BigInteger



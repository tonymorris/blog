---
comments: true
date: 2011-01-13 16:02:38
layout: post
slug: java-is-pass-by-value
title: Java is pass by value
wordpressid: 901
tags: Programming
---

I was linked to [this silly thread](http://www.theserverside.com/news/thread.tss?track=NL-461&ad=808081&thread_id=61622&asrc=EM_NLN_13145929&uid=2780877) today. I didn't read much of it and I recommend you don't either.

Instead, take note, it's yet another example of Java programmers knowing one language and knowing it poorly. This consistent observation is not particularly interesting, but this particular misunderstanding was cleared up _years_ ago. I went through an old Java FAQ database that I had written while I was working on the IBM implementation _in 2000_ where I explained all this. It was even asked in one of my wanky Java certifications back then. There were surely other explanations around at the time too. _Sigh_, Java guys, please... do yourself a favour and at least understand the shitty language that you hold so dear.

Java has two possible parameter types




  1. Object references


  2. primitives (restricted to 8 in total)



Note here that Java never passes objects, ever. They do not appear in the list above, double-check to make sure. Now, you don't have to take my word for it; after all, I'm just a former implementer of the language and API specification and I assure you, that doesn't change a thing with regard to credibility. However, just what was I implementing back then? It was a number of specifications; one of which was called The Java Virtual Machine Specification (VMS).

Here is [section 2.10.1 of the VMS](http://java.sun.com/docs/books/jvms/second_edition/html/Concepts.doc.html#26454). I will quote the whole section to make sure this point sinks in. If you are unsure whether Java passes by reference or value or some variation, then please read it again. Repeat this until you comprehend the important part. Here goes:



> 
The formal parameters of a method, if any, are specified by a list of comma-separated parameter specifiers. Each parameter specifier consists of a type and an identifier that specifies the name of the parameter. When the method is invoked, the values of the actual argument expressions initialize newly created parameter variables (§2.5), each of the declared type, before execution of the body of the method.

A method parameter of type float always contains an element of the float value set (§2.4.3); similarly, a method parameter of type double always contains an element of the double value set. It is not permitted for a method parameter of type float to contain an element of the float-extended-exponent value set that is not also an element of the float value set, nor for a method parameter of type double to contain an element of the double-extended-exponent value set that is not also an element of the double value set.

Where an actual argument expression corresponding to a parameter variable is not FP-strict (§2.18), evaluation of that actual argument expression is permitted to use values drawn from the appropriate extended-exponent value sets. Prior to being stored in the parameter variable, the result of such an expression is mapped to the nearest value in the corresponding standard value set by method invocation conversion (§2.6.8).




Let me extract a really important part:


> 
_...initialize newly created parameter variables (§2.5), each of the declared type, before execution of the body of the method. _




Did you see that? Java is pass by value, without reservation. It says so in the specification. It is even observable by writing a trivial program.


    
~~~{.Java}
Object o = new Object();
~~~



Now I know you may be accustomed to calling 'o' an object here, and it may even be the source of confusion, but it's not an object. It's an object reference. The object referred to by 'o' has no name.

When you pass 'o', it is copied, as per the **specification**, to a new reference, specified by the method being called. This means that if you reassign that reference, it cannot be observed outside of that method. Of course, since **it is a copy of the method parameter**.


    
~~~{.Java}
void method(Object o) {
  o = x; // not observable outside local scope
}
~~~



If you were to dereference 'o' you may be able to execute some effect that may be outside of the method. Note the adjective here, **dereference**. What are we dereferencing? An object reference of course! Surely you have seen a `NullPointerException`. Know what this means? You dereferenced a reference which held no object!

I don't know how else to make this clear, so I won't continue trying.

**Java is pass by value.**

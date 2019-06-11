---
comments: true
date: 2010-04-02 08:16:13
layout: post
slug: type-classes-are-nothing-like-interfaces
title: Type-classes are nothing like interfaces
wordpressid: 736
tags: Programming
---

A beginner is confused about Haskell's type-classes and so asks the question, "What is a type-class?" The response is often devastating, "You know, like Java or C# interfaces." This wildly misleading statement can leave the beginner in a state of disrepair. I'll tell you why but first I must emphasise.



> 
Type-classes are nothing like interfaces _(emphasis on nothing)_.




Haskell has something like interfaces. They are called data types and are expressed using the `data` or `newtype` keywords. Languages like Java/C# have nothing like type-classes; there is not even a close analogy. Consider for example, Java's `[Comparator](http://java.sun.com/javase/6/docs/api/java/util/Comparator.html)` interface. We would express this in Haskell like so:


    
~~~{.Haskell}
newtype Comparator a = C { compare :: a -> a -> Int }
~~~



Then if we wanted to sort a list, the type would be `sort :: Comparator a -> [a] -> [a]`. Notice the _explicit_ passing of the `Comparator` that would be required at the call site. This is just like Java.

Now suppose we did something that Java cannot do. We used a type-class.


    
~~~{.Haskell}
class Comparator a where
  compare :: a -> a -> Int
~~~



The type for sorting a list now becomes `sort :: Comparator a => [a] -> [a]`. This is just like the previous signature except for the way the left-most arrow is written. This is an important distinction. When the caller uses this function, it _implicitly_ passes the `Comparator`. Also, the type-class instance is decoupled from the data type. These are essential properties of type-classes. Indeed, it is its single-most defining property and since Java/C# have nothing like this, then it has nothing like type-classes.

Scala has implicit parameters which give you the ability to implement the essential property of type-classes (and more). Therefore, Scala does have something very much like type-classes. This is evident in a library such as [Scalaz](http://code.google.com/p/scalaz).

But let's try to save the idea.

Java has implicit type-conversion by virtue of inheritance. For example, a method that accepts a `T` can be passed a `U` and its implicit conversion to a `T` is denoted by the way of `U extends T`. Notice that no side-effect can be performed during this conversion. This is unlike Scala's implicit where it's simply a bad idea, not enforced (Scala also has inheritance like Java).

So, if you are to say type-classes are like anything in these languages, it's sort-of-like-yeah-ok-not-really inheritance. However, I'm sure you'll agree, this will only cause confusion for the poor beginner, so it's best not to draw the analogy at all.

Type-classes are a new concept to people coming from Java/C#. It is most appropriate to explain it as a new concept. I often use the contention between using `java.util.Comparable`, where the caller has the convenience of implicitly pass the implementation by way of inheritance but inconvenience of carrying the implementation with the data type versus using `java.util.Comparator` where the caller has the convenience of decoupling the implementation from the data type but requiring explicit passing at the call site. Type-classes (and Scala implicits) resolve this contention with a new concept.

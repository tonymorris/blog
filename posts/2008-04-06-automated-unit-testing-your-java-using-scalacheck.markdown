---
comments: true
date: 2008-04-06 10:03:28
layout: post
slug: automated-unit-testing-your-java-using-scalacheck
title: Automated Unit Testing your Java using ScalaCheck
wordpressid: 74
tags: Programming
---




### What is automated unit testing?



I'm assuming you're familiar with traditional mainstream unit testing techniques such as that purported by JUnit, NUnit and so on. Automated unit testing automates the generation of elements to apply a function to, in light of a stated _algebraic property of that function_. This is in contrast to traditional unit testing, where a proposition is made about the application of a function to specific elements of the domain.

Take, for example, the `java.lang.String.endsWith` function. Using JUnit, we might write:

    
    
      assertTrue("abc".endsWith(""));
      assertTrue("abc".endsWith("bc"));
      assertFalse("abc".endsWith("ab"));
      // ...and so on
    



Using automated testing, we might instead write something like this English statement:


> 
For any two Strings (x and y), then `(x + y).endsWith(y)` is `true`.




Here is that same statement for formally:


    
    
    ∀ x. ∀y. (x + y) endsWith y
    



Note that we don't have to declare x or y to be elements of the set `java.lang.String`, since the type system _proves_ this fact. Note also that we assume correctness of `+` as a premise of the proposition, while the proposition itself is _open_ (that is, this is the statement we are trying to disprove).

After putting forward the above open statement, the test software would automatically attempt to _falsify_ (note that this is different to "prove") this proposition by generating _arbitrary_ values. As a user, you may wish to skew the distribution of these values, adjust the amount of attempts at falsification and various other configuration values. Many have reasonable defaults, such as when generating integer values, include 0, 1, -1, max, min, max-1 and min-1 and typically make 100 to 500 attempts at falsification by default (in critical code, for example, you may turn this up).



### Credit where it is due



This concept of automated testing did not originate with [ScalaCheck](http://code.google.com/p/scalacheck/). In fact, I have recently noticed the mainstream fraternity have started replicating watered down versions of automated testing (note that it is extremely cumbersome to achieve effectively in mainstream languages). Indeed, advanced programming concepts often take a lot of time to bubble down from research/academia to mainstream in a typically diluted form. This concept originated from a paper titled [QuickCheck: Automated Specification-Based Unit Testing](http://www.cs.chalmers.se/~rjmh/QuickCheck/). Note that JUnit et. al. is not automated, but (very) manual testing.



### On with the story



The original language, Haskell, has language features called type-classes and higher-order functions, as does Scala. Both of these are required language features to use automated testing effectively, though it is possible to achieve in a lesser form in less useful languages. Note that Java does not have either of these language features. Are we hosed? No!

Take a look at this source file, which states our property about `endsWith` using Scala (see `endsWithProperty`):


    
~~~{.Scala}
// CheckString.scala

import org.scalacheck.Prop._
import org.scalacheck.ConsoleReporter.testStatsEx
import org.scalacheck.Test.check

object CheckString {
  // Here is the property

  val endsWithProperty = property((x: String, y: String) =>
    ((x + y) endsWith y)
  )

  // A List of properties - we only have one at the moment.
  val tests = scala.List(
      ("endsWithProperty", endsWithProperty)
  )

  // Run all our properties with the default configuration.
  // Are our properties false?
  def main(args: scala.Array[String]) =
    tests foreach { case (name, p) => testStatsEx(name, check(p)) }
}
~~~



You can run this source file by downloading [ScalaCheck-1.2.jar](http://scalacheck.googlecode.com/files/ScalaCheck-1.2.jar) and compiling it with the [Scala](http://www.scala-lang.org/downloads/index.html) compiler.


    
    
    $ scalac -classpath ScalaCheck-1.2.jar CheckString.scala
    



Does our stated property falsify?


    
    
    $ scala -classpath .:ScalaCheck-1.2.jar CheckString
    + OK, passed 100 tests.
    



No! After **100 unit tests**, the stated property cannot be shown to be false. We might assume its correctness now. Note that this is `java.lang.String` that we are testing here running on a Java Virtual Machine, not some other esoteric `String` -- yes, the one you use in your Java enterprise applications ;) You can apply this technique to **any Java type**.

How about this proposition:



> 
For any two Strings (x and y), then `x.reverse.startsWith(y.reverse))` is equivalent (in truth) to `x.endsWith(y)`.




Again, `startsWith` is assumed correct as a premise of the proposition (or theorem). Can the statement be falsified by the test automation?


    
~~~{.Scala}
// CheckString.scala

import org.scalacheck.Prop._
import org.scalacheck.ConsoleReporter.testStatsEx
import org.scalacheck.Test.check

object CheckString {
  val endsWithProperty = property((x: String, y: String) =>
    ((x + y) endsWith y)
  )

  // Can you falsify this?
  val endsWithProperty2 = property((x: String, y: String) =>
    x.endsWith(y) == x.reverse.startsWith(y.reverse))

  val tests = scala.List(
      ("endsWithProperty", endsWithProperty),
      ("endsWithProperty2", endsWithProperty2)
  )

  def main(args: scala.Array[String]) =
    tests foreach { case (name, p) => testStatsEx(name, check(p)) }
}
~~~



Well, can it? Oh come on, please tell me! :)


    
    
    $ scala -classpath .:ScalaCheck-1.2.jar CheckString
    + OK, passed 100 tests.
    + OK, passed 100 tests.
    



No. We have now successfully executed 200 unit tests so we may be prepared to declare satisfaction with the correctness of the propositions under analysis. Note again, that we have not _proven_ these properties to be true (we did prove others to be true using the type system). In fact, doing so for the general program is equivalent to solving the halting problem, so good luck if you're looking for something more rigorous ;)

You may have observed that I called a method on `String` that does not exist in the Java API -- reverse. In Scala, you can add methods to objects in a type-safe manner. The `reverse` method is added by default in the Scala runtime. In Java, you might instead have written reverse as a `static` method of a `StringUtils` class or something. This fact does not negate the fact that we are still using the same ol' `java.lang.String` that Java Jim is using next door.



### What were the String values?



The test automation supplied 100 `String` values for each of the two properties in an attempt to falsify; where did they come from? ScalaCheck includes an abstract type called `[Arbitrary](http://scalacheck.googlecode.com/svn/artifacts/1.2/doc/api/org/scalacheck/Arbitrary.html)` and an implementation for the `String` type is supplied by ScalaCheck. It is this that is used to generate `String` values. If we wish to generate values for our own types, we must write our own `Arbitrary` implementation for it.

I will state some properties now about `java.util.LinkedList` and its `addFirst` method. However, note that unlike `java.lang.String`, ScalaCheck does not include an arbitrary generator for linked lists, so we will have to write one. Luckily, generators are instances of what is known as a _functor_ (also, a _monad_), so this task will be quite trivial. Let's start with doing that.

We can generate arrays, since that ability comes with ScalaCheck, so we can use the `map` function (i.e. the essence of a functor) to take the generator for arrays to a generator for linked lists. Given an array, we can create a linked list using: `new LinkedList[A](Arrays.asList(array))`. This is precisely what the code below does using what is called a _higher-order function_ inside the `Arbitrary` functor. You needn't concern yourself with these advanced programming concepts in these demonstrations, but just acknowledge their existence and that the given code is trivial and it works.


    
~~~{.Scala}
import org.scalacheck.Arbitrary
import org.scalacheck.Arbitrary.arbitrary
import java.util.{LinkedList, Arrays}

object CheckLinkedList {
  implicit def arbitraryLinkedList[A](implicit a: Arbitrary[A]): Arbitrary[LinkedList[A]] =
    Arbitrary(arbitrary[Array[A]].map(array => new LinkedList[A](Arrays.asList(array))))
}
~~~



Note the `implicit` declaration on the method. This is _required_, however, this language feature is very powerful and only a very lengthy discussion would do it justice, so we will say nothing more than it is required in this case, it is very powerful and Java has no equivalent (you can mildly emulate it with the singleton anti-pattern -- another topic).

What properties can we state now about `addFirst`? How about for any list and any element, adding the element to the list adds 1 to its `size`? We call this property `addsLengthOne` below.


    
~~~{.Scala}
// CheckLinkedList.scala

import org.scalacheck.Arbitrary
import org.scalacheck.Arbitrary.arbitrary
import java.util.{LinkedList, Arrays}
import org.scalacheck.Prop._
import org.scalacheck.ConsoleReporter.testStatsEx
import org.scalacheck.Test.check

object CheckLinkedList {
  implicit def arbitraryLinkedList[A](implicit a: Arbitrary[A]): Arbitrary[LinkedList[A]] =
    Arbitrary(arbitrary[Array[A]].map(array => new LinkedList[A](Arrays.asList(array))))

  val addsLengthOne = property((list: LinkedList[String], s: String) =>
    list.size + 1 == { list.addFirst(s); list.size }
  )

  val tests = scala.List(
      ("addsLengthOne", addsLengthOne)
  )

  def main(args: scala.Array[String]) =
    tests foreach { case (name, p) => testStatsEx(name, check(p)) }
}
~~~



_I've copied and pasted the boilerplate to get the tests running for the demonstration, but please feel encouraged to put this kind of code in its own function to avoid this repetition._

How did we go?


    
    
    $ scala -classpath .:ScalaCheck-1.2.jar CheckLinkedList
    + OK, passed 100 tests.
    



Yippee!!

We might state two more properties:





  * 
  For any list and any element (s), `list.add(s)` then `list.get(0)` will always yield `s`.



  * 
  For any list, any element (s) and any integer between 0 and list.length - 1 (n), then calling `list.get(n)` is equivalent to calling `list.addFirst(s).get(n + 1)`.





    
    
    $ scala -classpath .:ScalaCheck-1.2.jar CheckLinkedList
    + OK, passed 100 tests.
    + OK, passed 100 tests.
    + OK, passed 100 tests.
    



I have left off the code for the moment and I will give a complete example toward the end. Note that we have 300 passing unit tests -- the code is around 25 lines (excluding the boilerplate to get the tests running).

At this point, we have given **an unambiguous specification for `addFirst`**. In other words, it is not possible to write a terminating function that does not satisfy these properties that is not equivalent to `addFirst`. However, this ignores the unfortunate potential for uncontrolled side-effects, which is an inherent property of Java (and Scala for that matter). We will assume it to be the case -- no additional side-effects. It then follows that one additional benefit of automated testing is that we also obtain extremely valuable documentation for the function.

Consider being provided with:




  * 
  The type signature for `addFirst` (the properties which are proved).



  * 
  The three stated properties for `addFirst`.




Is there anything more useful to add in terms of documentation? Isn't this unambiguous, highly readable specification more than you could possibly ask for? This is an interesting question to ponder, particularly in a more pure environment than the one we are discussing here, but I thought I would mention it anyway :)

I will state a property that is false, just to exemplify what happens. The idea is that it is extremely difficult (but not impossible -- remember halting problem) to state a false property that ScalaCheck passes. I'm not going to look for a specific corner case that traditional unit testing is unlikely to pick up, but I encourage you to do so as an added exercise.

Instead, I will state an obviously false property; for any list and any element, adding the element with `addFirst` is equivalent to adding that element with `addLast`.

Here we go (note the fourth property -- `thisIsFalse`):


    
~~~{.Scala}
// CheckLinkedList.scala

import org.scalacheck.Arbitrary
import org.scalacheck.Arbitrary.arbitrary
import java.util.{LinkedList, Arrays}
import org.scalacheck.Prop._
import org.scalacheck.ConsoleReporter.testStatsEx
import org.scalacheck.Test.check

object CheckLinkedList {
  implicit def arbitraryLinkedList[A](implicit a: Arbitrary[A]): Arbitrary[LinkedList[A]] =
    Arbitrary(arbitrary[Array[A]].map(array => new LinkedList[A](Arrays.asList(array))))

  val addsLengthOne = property((list: LinkedList[String], s: String) =>
    list.size + 1 == { list.addFirst(s); list.size }
  )

  val getZero = property((list: LinkedList[String], s: String) =>
    { list.addFirst(s); list.get(0) == s }
  )

  val getPlusOne = property((list: LinkedList[String], s: String, n: Int) =>
    (n >= 0 && n < list.size) ==>
    (list.get(n) == { list.addFirst(s); list.get(n + 1) })
  )

  val thisIsFalse = property((list: LinkedList[String], s: String) =>
    {
      val listt = list.clone.asInstanceOf[LinkedList[String]]
      list.addFirst(s)
      listt.addLast(s)
      list == listt
    }
  )

  val tests = scala.List(
      ("addsLengthOne", addsLengthOne),
      ("getZero", getZero),
      ("getPlusOne", getPlusOne),
      ("you're a big fat liar!", thisIsFalse)
  )

  def main(args: scala.Array[String]) =
    tests foreach { case (name, p) => testStatsEx(name, check(p)) }
}
~~~



And when we run:


    
    
    $ scala -classpath .:ScalaCheck-1.2.jar CheckLinkedList
    + OK, passed 100 tests.
    + OK, passed 100 tests.
    + OK, passed 100 tests.
    ! Falsified after 2 passed tests:
    > ARG_0 = "[, a, b]"
    > ARG_1 = ""
    java.lang.Error: you're a big fat liar!: Failed(List(Arg(,[, a, b],0), Arg(,,0)))
    



Our usual three properties pass just fine, but notice that ScalaCheck prints the _counter-example_ to the false property. We can determine that this will never be the empty list since the property holds for the empty list (does it? don't just take my word for it!). ScalaCheck likely tries the empty list one or two times before it declared to have "Falsified after 2 passed tests" using a non-empty list.

Imagine if ScalaCheck had printed a list of length 20 or 30. If our property was particularly complicated, it would be more difficult to reason about our code and the property to determine _why what we stated was false_. If the property is also false for a list of length 10, we'd probably want to see that counter-example instead to make debugging easier. Indeed, it would be even easier to have the shortest possible list length that still falsifies what we believed to be correct. This concept is known as _shrinking_ and ScalaCheck certainly provides this ability -- occasionally with some assistance from the user for user-defined types -- a bit like having to declare your own `Arbitrary`, you also declare your own shrinking strategy and this is also typically trivial.

In our case, the linked list was not shrunk, since we hadn't provided a shrinking strategy for linked list (and one is not supplied with ScalaCheck), so the first counter-example was used. In non-demonstration environments, it is encouraged to put this additional effort in to your testing practices by supplying a shrinking strategy for user-defined types (if possible and sensible -- it is not always so).



### Conclusion



Automated unit testing is an incredibly important tool to any programmer -- even when using Java. It serves as rigorous documentation at the very worst and more typically, as a robust unit testing platform (300 unit tests in 25 lines in a demo -- it's usually even better than that! [here is over 30,000 unit tests](http://projects.workingmouse.com/public/scalaz/tags/2.4/src/test/scalaz/CheckEither.scala)).

Coming up with good properties is an extremely (this adverb cannot be exaggerated) disciplined task and I implore any programmer to strive to do it effectively. Note that stating one property may imply others, so stating those others is potentially redundant. In other words, it is the set of properties that make up the unit for analysis -- you cannot necessarily declare the legitimacy of each one independently.

This form of testing is a physical manifestation of an integral part of computer programming -- the formation of the logical theorem (see Curry-Howard Isomorphism) that makes up the program. We conclude by noting that this more disciplined, robust and formal approach to programming requires considerably less effort than traditional unit testing techniques, but rigorous mental discipline. It can also be observed that this approach de-emphasises the English description (i.e. function name, javadoc, etc.) as a meaningful method of extrapolating function behaviour.

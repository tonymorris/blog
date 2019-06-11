---
comments: true
date: 2008-06-17 15:39:04
layout: post
slug: tests-as-documentation
title: Tests as Documentation
wordpressid: 86
tags: Programming
---

[_(Copied from)_](http://wiki.workingmouse.com/index.php/Tests_As_Documentation)



#### Wouldn't it be nice if?



When disciplined programmers write unit tests, they often make reference to the fact that their tests provide a means of documentation the software that it is testing. This documentation is more appropriate than what would otherwise be informal and potentially ambiguous comments using English. Take the simple example of adding two numbers. We might document using informal language:


    
~~~{.Java}
/**
 * Adds the two arguments.
 *
 * @param a Add this argument to the other one.
 * @param b Add this argument to the other one.
 * @return The sum of the two arguments.
 */
~~~



With unit tests, we might instead write something more formal and unambiguous:


    
~~~{.Java}
assertEqual(add(2, 2), 4)
assertEqual(add(4, 3), 7)
~~~
    ... so on
    



It might be argued that both these forms of documentation complement each other. After all, while the unit tests have less room for misinterpretation, they are incomplete; for example, what about `add(88, 37)`? The English description makes up for this shortcoming.

We could reword our English to be a little more succinct:


    
~~~{.Java}
/**
 * Passing 0 as one argument returns the other argument,
 * otherwise, the result is the same as subtracting 1 from one argument and
 * adding 1 to the other argument then passing those values instead.
 * e.g. add(2, 8 ) is the same as add(1, 9) and so on until one of the arguments reaches 0.
 */




Wouldn't it be nice if we could express this formally in unit tests? You can, read on.

While this example is trivial, it scales in proportion to the amount of discipline that the programmer is willing to exercise by _controlling side-effects_ in their program. If we write our programs such that most of our methods retain the property of _referential transparency_, we can use this advanced method of tests as documentation. When we refactor our code to make tests easier to write, it is often the case that we are doing exactly this anyway. Win win!



#### Let's scale up a little



We'll give a slightly less trivial example next, but not so trivial that it takes away from the important points. In fact, let's unit test a specific part of the Java Collections library -- the `java.util.Collections.reverse` method. There are various ways of testing this method and we will choose one here that serves to illustrate the point of unit tests as documentation.

The `reverse` method can be described as follows:




  1. For the empty list, then reversing this list is always the same list


  2. For the list with one element, then reversing this list is always the same list


  3. For any other two lists (let's call them 'a' and 'b'), then appending b to a then reversing will yield the same list as reversing a, then appending the result to the reverse of b. Since this statement is a little convoluted, let's write it with some pseudo-Java syntax notation: `(a.append(b)).reverse() == b.reverse().append(a.reverse())`



It is an interesting observation here that we have _completely specified_ the `reverse` method. That is, under some reasonable assumptions, it is not possible to write a method that is not equivalent to `reverse` that also satisfies our statements above. This is the ultimate form of code documentation!

We will ignore the first statement for the sake of interest and verbosity and focus on expressing the other two. This is because statements 2 and 3 have _free variables_, while statement 1 is merely an assertion that does not illustrate any interesting points. Let us start with the second statement and articulate it using [Reductio](http://reductiotest.org/):

    
~~~{.Java}
    Property p2 = property(arbInteger, new F<Integer, Property>() {
      public Property f(Integer i) {
        return prop(single(i).equals(reverse(single(i))));
      }
    });
~~~



That pretty much sums up statement 2 doesn't it? What about statement 3:


    
~~~{.Java}
Property p3 = property(arbLinkedList(arbInteger), arbLinkedList(arbInteger), new F2<LinkedList<integer>, LinkedList<integer>, Property>() {
  public Property f(LinkedList<integer> a, LinkedList<integer> b) {
    final LinkedList<integer> x = reverse(append(a, b));
    final LinkedList<integer> y = append(reverse(b), reverse(a));
    return prop(x.equals(y));
  }
});
~~~





#### Is that it?



Yep. Notwithstanding the absence of statement 1, we have completely specified the behaviour for the Java `Collections.reverse` method. We have exhaustive and formal documentation instead of one or the other as we traditionally do. What an improvement!



#### Yeah but I want to unit test it too



That's not hard either. How many unit tests do you want to run? By default, Reductio will run 100 unit tests per `Property` declaration. You can adjust this and various other factors about how your unit tests are executed. If you want to take the default, then a few more lines of code are enough to do just that:


    
~~~{.Java}
list(p2, p3).foreach(new Effect<property>() {
  public void e(Property p) {
    summary.println(p.check());
  }
});
~~~



If you run this line of code, you will see the result of your 200 unit tests on the standard output:


    
    
    OK, passed 100 tests.
    OK, passed 100 tests.
    



Is that too magical for you? Don't believe me? Want to see it fail? OK, let's fail it. In the expression of statement 3, change the line `b.addAll(a)` to `a.addAll(b)` and run again. What did you see? Here is what I saw:


    
    
    OK, passed 100 tests.
    Falsified after 4 passed tests with arguments: [[3, 2, -3, 4, -3],[2, -3, 4, -3]]
    



Yep, it failed alright :) When those two list values are used as our free variables, the property is false and the unit test fails.



#### Other Resources






  * [A Case for Automated Testing](http://wiki.workingmouse.com/index.php/A_Case_for_Automated_Testing)


  * [Reductio EqualsHashCode](http://wiki.workingmouse.com/index.php/Reductio_EqualsHashCode)


  * [Reductio Website](http://reductiotest.org/)


  * [More Reductio examples](http://reductiotest.org/examples)


  * [Reductio Manual](http://reductiotest.org/manual)




#### Complete Runnable Source Code




    
~~~{.Java}
import fj.Effect;
import fj.F;
import fj.F2;
import static fj.data.List.list;
import static reductio.Arbitrary.arbInteger;
import static reductio.Arbitrary.arbLinkedList;
import static reductio.CheckResult.summary;
import reductio.Property;
import static reductio.Property.prop;
import static reductio.Property.property;

import java.util.Collections;
import static java.util.Collections.singletonList;
import java.util.LinkedList;

public class ListReverse {
  public static void main(String[] args) {
    Property p2 = property(arbInteger, new F<Integer, Property>() {
      public Property f(Integer i) {
        return prop(single(i).equals(reverse(single(i))));
      }
    });

    Property p3 = property(arbLinkedList(arbInteger), arbLinkedList(arbInteger), new F2<LinkedList<integer>, LinkedList<integer>, Property>() {
      public Property f(LinkedList<integer> a, LinkedList<integer> b) {
        final LinkedList<integer> x = reverse(append(a, b));
        final LinkedList<integer> y = append(reverse(b), reverse(a));
        return prop(x.equals(y));
      }
    });

    list(p2, p3).foreach(new Effect<property>() {
      public void e(Property p) {
        summary.println(p.check());
      }
    });
  }

  static <a> LinkedList<a> single(A a) {
    return new LinkedList<a>(singletonList(a));
  }

  static <a> LinkedList<a> reverse(LinkedList<a> as) {
    LinkedList<a> aas = new LinkedList<a>(as);
    Collections.reverse(aas);
    return aas;
  }

  static <a> LinkedList<a> append(LinkedList<a> as1, LinkedList<a> as2) {
    LinkedList<a> aas = new LinkedList<a>(as1);
    aas.addAll(as2);
    return aas;
  }
}
~~~

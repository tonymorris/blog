---
comments: true
date: 2008-06-06 15:58:53
layout: post
slug: a-case-for-automated-testing
title: A Case for Automated Testing
wordpressid: 85
tags: Programming
---

[_(Copied from)_](http://wiki.workingmouse.com/index.php/A_Case_for_Automated_Testing)



#### Discipline and Testing


Disciplined programmers write tests while developing code. Some even refactor their tests when certain situations arise. Imagine this one. You have just written a method that takes two integer arguments, adds them and subtracts seven before returning the result.

Your code might look like this:

    
~~~{.Java}
int addThenSubtract7(int x, int y) {
  return x + y - 7;
}
~~~



When you test this code -- either before, during or after you write the method above -- you will likely write tests that exhibit some property about the method under test. For example, when one argument is the negation of the other, you will always get back `-7`.


    
~~~{.Java}
assertEquals(-7, addThenSubtract7(0, 0));
assertEquals(-7, addThenSubtract7(1, -1));
assertEquals(-7, addThenSubtract7(2, -2));
assertEquals(-7, addThenSubtract7(3, -3));
assertEquals(-7, addThenSubtract7(4, -4));
assertEquals(-7, addThenSubtract7(5, -5));
~~~



You might refactor in this scenario:


    
~~~{.Java}
assertTrue(negationBecomesNegative7(0));
assertTrue(negationBecomesNegative7(1));
assertTrue(negationBecomesNegative7(2));
...

public boolean negationBecomesNegative7(int a) {
  return addThenSubtract(a, -a) == -7;
}
~~~



Indeed, you might use a loop for your input values:


    
~~~{.Java}
for(int i = 0; i < 100; i++) {
  assertTrue(negationBecomesNegative7(i));
}
...
~~~







#### Specifying Properties


There are other properties about the method `addThenSubtract7`. For example, if you switch the arguments, the result is unaffected. In fact, this property comes up a fair bit so it even has a name; _commutativity_. You could come up with a similar scenario as that described above through a process of repeating assertions with different values, refactoring a bit and generally trying to test as rigorously as possible with minimal effort and permitting future maintainability of both your code and your tests.

Eventually, you will have written properties about your method such that there is no reason to write any more properties, since they would be implied by your existing properties. For example:





  * You could write a test for `addThenSubtract7(0, anyValue)` results in `anyValue - 7`.


  * Then you could write a test for commutativity; `addThenSubtract7(someX, someY)` results in the same value as `addThenSubtract7(someY, someX)`.



Now, is there any point writing a test for `addThenSubtract7(anyValue, 0)` results in `anyValue - 7`? Isn't this implied by the test for commutativity? Right, this test would be redundant.

Ultimately, you could aim for an _unambiguous specification_ of your method under test. However is there a nicer way to write all these tests?




#### Let's Automate it!



Going back to the first property `negationBecomesNegative7`, don't you really just want to say this and then let the rest be taken care of by clever automation?

    
    
    for all int values named a. then addThenSubtract7(a, -a) == -7
    



This is the job of _Automated Specification-based Testing_ such as that which is implemented by [Reductio for Java](http://reductiotest.org/). Reductio allows you to write the above expression using standard Java syntax:


    
~~~{.Java}
Property p = property(arbInteger, new F<integer, Property>() {
  public Property f(final Integer a) {
    return prop(addThenSubtract7(a, -a) == -7);
  }
});
~~~



What is going on here? We are passing in our generator of arbitrary integer values (`arbInteger`), then an anonymous inner class that asserts our property for our _free variable_ (the one called 'a').

OK, it can't be this easy can it? No, sorry, it can't. There is an enormous of work to do next. Ready, here it is:

    
    
    p.check();
    


With all that heavy lifting taken off you by the test automation, you should now have plenty of time and energy to get on with delivering your software to your client or maybe reading news blogs if that is what you prefer :)




#### Shrinking



When you run the test automation, you receive a result of success after running 100 (by default -- adjustable of course) tests or you receive a failed result with a counter-example. That is, you receive a value for your free variable (a) for which the property was not true.

Does it stop here? I mean, this is great and everything, but surely there's not more help that can be provided by clever test automation software? There are a couple more clever things that can be done.

First is the _shrinking of a counter-example_. Imagine there was a bug in the `addThenSubtract7` method and you received a counter-example of `1789234`. What's so special about this number? Well, that depends on the bug, however, maybe this is just the first counter-example that was found. Imagine, however, that the bug also exhibited itself (i.e. the property fails) with a counter-example of `2`. Isn't this a more useful counter-example? Wouldn't you prefer that this one was reported and not that big, insignificant number?

So that's another great feature of test automation; it can report back to you _useful counter-examples_ in the event of a bug. When you perform manual testing as above, you might remove a couple of your assert methods to help narrow down the bug. This is the hard way! This very act can be automated.

Imagine you wrote this property (`a + b == a - b`):

    
~~~{.Java}
final Property p = property(arbInteger, arbInteger, new F2<integer, Integer, Property>() {
  public Property f(final Integer a, final Integer b) {
    return prop(a + b == a - b);
  }
});
~~~



It turns out that this property is true for some values such as a == 0 and b == 0, but false for others:




  * `a = 1, b = 15`


  * `a = 147, b = 92582`


  * `a = 5923, b = 12`



But which of these counter-examples is going to be most useful to you, the tester? Any of the above? How about this counter-example: `a = 0, b = 1`? Isn't this more useful to you than any of those above?

Good test automation software will report a reasonably small counter-example in the event of the falsification of a property.




#### Automated Mocking



OK, is there more? Yes sorry to keep your attention. Test automation software will also generate **instances of an interface** for you. That's right, you can _quantify across an interface_ in your property expressions. This is a bit like mocking, except not so manual; you just pass in the arbitrary generator (Reductio provides them of course) just as we did above and you can wipe your hands clean of the remaining manual effort otherwise.

You might want to write a property where you say something like, "for all instances of my interface or class, then such and such...". This might be an instance of the `HttpServletRequest` interface or perhaps one of your own classes. The good part is, no more manual mocking.




#### Want to Learn More?



You can read the [Reductio Manual](http://reductiotest.org/manual) or perhaps look at some [code examples](http://reductiotest.org/examples/1.5). If you're a fan of the coming Java 7 BGGA closure syntax, you might prefer [these code examples](http://reductiotest.org/examples). Perhaps you're into higher-level programming and prefer [Scala code examples](http://reductiotest.org/examples/scala). Whichever you choose, please feel free to jump on the [Reductio mailing list or chat channel](http://reductiotest.org/community) and ask your question, no matter how silly it seems to you.

---
comments: true
date: 2006-11-07 14:57:27
layout: post
slug: have-you-ever-wanted-to-do-this
title: Have you ever wanted to do this?
wordpressid: 8
tags: Programming
---

You're working away on your Java (or .NET for that matter) project and you want to redefine the equality for some type; let's say, for a String. The new requirement mandates that two Strings are "equal" as per usual except that if they have a length greater than or equal to 3, then it is only the first 3 characters that need to be equal for the Strings to be equal. For example:



	
  * "abcdef" and "abcxyz" are equal

	
  * "abc" and "abcdef" are equal

	
  * "ab" and "abc" are **not** equal

	
  * "ab" and "ab" are equal

	
  * "" and "a" are **not** equal

	
  * "" and "" are equal



Make sense so far? Good! :) If our problem was related to the ordering of instances of our type, we already have `Comparator` in the core API to cater for this scenario and best of all, the collections supports the use of our _comparing strategy_ e.g. we can pass an instance to the constructor of `TreeSet`. However, our problem is related to testing for equality, not ordering. How would the analogous Comparator interface look for equality? How about this:


    
~~~{.Java}
interface Equalable<t> {
    boolean isEqual(T t1, T t2);
}
~~~



It's too bad the collections don't support this type, ain't it? We can't plug in our own _strategy for testing for equality_ like we can for comparing for order if we intend on using collections. Instead, the only way we can meet our requirement and still use collections (or anything that uses the inherent `equals` method) is by declaring a new type:


    
~~~{.Java}
public final class XString
{
  private final String str;

  public XString(final String str) {
    this.str = str;
  }

  public String getStr() {
    return str;
  }

  public boolean equals(final Object o) {
    ...
  }
}
~~~



Now  we can have collections of this type as it defines its own equality test strategy in accordance with our original requirement. What if the collection types allowed you to pass an instance of `Equalable` instead? Much easier than having to write our `XString` type!? Instead of having to create the `XString type`, we might be creating `HashSet` instances like so `new HashSet(new MyEqualable());` and be done.

Let's take a look at how this would look in another programming language - with [Haskell](http://haskell.org/) - a _purely functional_ programming language. In Haskell, we have the same two choices, we might implement the type-class [Eq](http://haskell.org/ghc/docs/latest/html/libraries/base/Prelude.html#t%3AEq) or we might pass a _first-class function_ of type a -> a -> Bool


    
    
    newtype XString = XString String
    
    instance Eq XString where
      ...
    



Now, if we had a function of type `a -> b` _[see note 1]_ and that function needed a strategy for equality over the type `a` to evaluate, we have two choices:



  
  1. we could bound the parameter by the `Eq` type-class, which would look like so as a function definition `Eq a => a -> b` and reads as "takes an argument of type a, where a is bound by the type-class Eq, and evaluates to an unbound type b"

  
  2. we could leave the parameter `a` unbound and pass a function as an argument. This function would be of type `(a -> a -> Bool)` and would define the strategy for determining if two _a's_ are equal. Thus our entire function definition would look like so `(a -> a -> Bool) -> a -> b ` where we see that `a` is not bound by any type-class i.e. it is unbound.



Back into Java land... these two options in Haskell can be likened to `java.lang.Comparable` and `java.util.Comparator`. Suppose now you have a reference of type `Set` and you wish to call an existing method of type `m(Set)`. It is only sensible that you are able to call this method, since (let's assume) it has nothing at all to do with the only differentiator between the two types - the strategy for determining equality. Gotta write a new type right?

Now, since this is my first blog post ever, I reserve the right to throw out some incredibly bold and apparently absurd postulations :). Since such a simple requirement mandates the creation of so much code to fulfill accurately, I postulate, with more formal reasons forthcoming, that approximately 99.9999% of the content of the J2SE API Specification [see note 2] is a result of this problem that is inherent in a language such as Java with a substandard type system. I might name this problem, "Gotta write a new type, right?" for future reference and just to take the piss out of the dominant programming language(s) of our evolving _[see note 3]_ industry. So that's its name, OK? Great.

I do not envy you oh Senior J2EE Architect/.NET Super-duper Spanky Star (what do they call those anyway?).

[1] This can be read as "takes an argument of type a and evaluates to a type b where a and b are both polymorphic and unbound". The analogous Java is:

    
~~~{.Java}
interface I<a, b> {
  b f(a arg);
}
~~~



[2] The "thing" (monolithic abominable creature?) that I used to laboriously implement for IBM [http://java.sun.com/j2se/1.5.0/docs/api/](http://java.sun.com/j2se/1.5.0/docs/api/)

[3] I must have backspaced 50 or so times before finally settling on the conservative adjective, "evolving" :)

---
comments: true
date: 2007-02-15 16:51:42
layout: post
slug: refunctoring
title: Refunctoring
wordpressid: 24
tags: Programming
---

Ask your average Java (or C#) programmer to 'write a method that takes a list of integers, adds 10 to each element, converts the result to a String, prepends *** to it and returns the resulting list'. It's quite easy and you can certainly postulate what kind of start would be made on this. Here is how I suspect most Java programmers would solve this problem:

    
~~~{.Java}
...
static List<string> addTenAndConvert(List<integer> list) {
  List<string> result = new LinkedList<string>();

  for(Integer i : list) {
    String s = "***" + String.valueOf(i + 10);
    result.add(s);
  }

  return result;
}
~~~


Simple enough. Many people would also write so-called 'unit tests' to assist in verifying that the method meets the specified requirement. It is interesting to note at this point that 'for all' ([hint](http://en.wikipedia.org/wiki/Universal_quantification)) list arguments, the length of the return value will always be equal. So for the argument, {1,2,3} with a length of 3, the result is {"***11", "***12", "***13"} also with a length of 3 and this property holds across all lists.

But then, I ask for another method that does all the same, but this time, prepends "AAA" instead of "***". Whatchya gunna do? Copy/paste/change? No, you're going to refunctor (to be defined in time to come), that's what. You might pass an additional argument of type String and prepend that. Sounds fair enough. But then I will ask instead that you append "BBB" instead of prepending "AAA". Pass an additional argument of type boolean?

Let's take a look at what we've got:

    
~~~{.Java}
...
static List<string> addTenAndConvert(
  List<integer> list, String s, boolean prepend) {
  List<string> result = new LinkedList<string>();

  for(Integer i : list) {
    String ss = prepend ? s + String.valueOf(i + 10) :
        i.toString() + s;
    result.add(ss);
  }

  return result;
}
~~~


But then, I ask you for another requirement -- this time devastating. I ask instead that it is not a List argument, but a List argument and a List return type. Going to copy/paste this time, right? The conversion from each String element to the new Integer element is left unspecified for now, but feel free to dream something up. In fact, feel free to 'refactor' it out:

    
~~~{.Java}
interface Convert {
  Integer convert(String s);
}
~~~


...then change the method like so:

    
~~~{.Java}
...
static List<integer> stringListToIntegerList(
    Convert c, List<string> list) {
  List<integer> result = new LinkedList<integer>();

  for(String s : list) {
    Integer i = c.convert(s);
    result.add(i);
  }

  return result;
}
~~~


Notice that our method's name is becoming less specific as it is refactored and becoming more abstract in behaviour. Further, we cannot write our method `addTenAndConvert` in terms of our method `stringListToIntegerList` by passing a different implementation of `Convert` because our types do not match. However, notice that the transformation on each list has nothing to do with specific types -- these types are actually _unbounded polymorphic_ types. Let's try again.

    
~~~{.Java}
interface Convert<T, U> {
  U convert(T t);
}

static <T, U> List<u> fooForNow(Convert<T, U> c, List<t> list) {
  List<u> result = new LinkedList<u>();

  for(T t : list) {
    U u = c.convert(t);
    result.add(u);
  }

  return result;
}
~~~


How's that look!!? What shall we call this method? I have called it `fooForNow`, but if you'll just let me call it `map` instead, just because. We can now write our method `addTenAndConvert` with our newly found abstraction -- the `map` function:

    
~~~{.Java}
static List<string> addTenAndConvert(List<integer> list) {
  Convert<Integer, String> c = new Convert<Integer, String>() {
    public String convert(Integer i) {
      return "***" + String.valueOf(i + 10);
    }
  };

  return map(c, list);
}
~~~


How funky is that? Funky enough for you to start looking at funktional programming? I'll let you in on a little secret. This relatively high euphoric point of using Java/C# (et. al.) is the very basis and starting point of most functional programming languages. In fact, the `map` function is typically included in the standard libraries! Functional programming is Java version 42, seriously. It is a natural extension to what it is that most Java/C# programmers are already doing. It is not some esoteric, orthogonal, unrelated paradigm that has nothing to do with anything except the point of singularity.

If we consider our `Convert` type to actually represent 'anything that can convert a T to a U', we call `map` a higher-order function, since it takes a function (from T to U) as an argument. Whether or not to name this function argument (to `Convert` in our example) and hide the behaviour of this conversion behind some implementation sets the premise for the 'DD (Dynamic Dispatch) versus HOF (Higher-Order Function) issue'.

If we look at the type for the Haskell `map` function, we see it like this:

    
~~~{.Haskell}
map :: (a -> b) -> [a] -> [b]
~~~


That is, as the first argument, it takes a function from a type 'a' to a type 'b' (we called these T and U, but Haskell type parameters must be lower-case). The second argument is a list of the type 'a' and the return type is a list of the type 'b'. That's exactly what we just wrote in 43 trillion lines of Java code! OK, maybe I am exaggerabating a bit there.

And in case you're wondering, here is how `addTenAndConvert` looks in Haskell:

    
~~~{.Haskell}
let addTenAndConvert = map (("***" ++) . show . (+10)) -- tidy eh?
~~~



Why 'Refunctoring'? First, I refuse to reuse euphemistic terms that have served only to denigrate the standard of the software development industry, therefore, I cannot use 'Refactoring' (oooh! did I just say that!!?). To show this, consider the fact that the entire notion of 'Refactoring' is done away with one simple phrase 'Functional Programming' in the case shown and hundreds more. Of course, you might decide to redefine Refactoring to a different context, but then, redefining terms often leads to confusion (the term 'function' is itself a perfect example). To further demonstrate this point, open [the book](http://www.amazon.com/Refactoring-Improving-Design-Existing-Code/dp/0201485672) or [catalog](http://www.refactoring.com/catalog/index.html) on Refactoring and turn to the section titled, 'Replace Parameter with Method'. Then say out loud, 'partial application' -- if you don't know why you're saying that, learn what partial application is, then perhaps it might dawn on you. A useful exercise is to apply this technique to all so-called 'Refactorings' and see how many are eliminated by existing programming techniques that don't get very much air time in advertising material (though, this might be changing thankfully). More useful might be to see how many are left standing (want to know my conjecture?).

Anyway, Refunctoring is what most people **really are doing**. They are converting their code that is written in not-too-powerful programming languages to mimic the power that is already available in functional programming languages. They are representing higher-order functions with (_importantly, co-variant_) polymorphic type parameters, refactoring out what partial application already provides and much more. This is being done, despite having been invented many, many years ago, but the fact that it _is_ being done mandates a name, albeit how absurd it might be. I use the term Refunctoring, for no other reason than it was suggested to me by a bemused colleague (you know who you are :)) one day and upon reflection, I have decided that it is entirely appropriate.

You don't think the `map` function is a special case do you?

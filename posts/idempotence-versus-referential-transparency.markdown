---
comments: true
date: 2007-07-05 13:27:09
layout: post
slug: idempotence-versus-referential-transparency
title: Idempotence versus Referential Transparency
wordpressid: 33
tags: Programming
---

There is [a lot of misunderstanding about the difference between idempotence and referential transparency](http://programming.reddit.com/info/124kj/comments/c127nc). My plan here, is to explain the key differences without going into any specific detail about either concept. I plan to write something about referential transparency in the future and when I do, I will refer here to dispel any myths regarding idempotence.

I will assume that my reader is roughly familiar with the concept of referential transparency, but not necessarily its implications (since there are many), even though some [clearly demonstrate a lack of understanding of this concept](http://kawagner.blogspot.com/2007/01/real-functional-programming-or-why-io.html), strangely accusing others of same (go figure?) and even trying to [proclaim that Java has higher-kinds while suggesting that others do not understand Java's (mediocre) type system](http://blog.tmorris.net/strong-type-systems/#comment-1851). Ignorance is bliss and all that, anyway on with the story...

Here is a fact; all idempotent functions are also referentially transparent, but not necessarily the other way around. Let us express this fact by other means; a referentially transparent function is _not necessarily_ idempotent, because it is _(only) possibly_ idempotent.

Idempotence can be easily written as follows. A function (f) is idempotent if the following property holds:


> 
f o f = f



That's all there is to it. Since many of my audience are Java programmers, I will appeal to their potential misunderstanding here and elaborate. The o symbol means _function composition_. The statement can be read as "the function f composed with the function f is equivalent to (just) f".

Function composition comes about by... well composing functions. Suppose a function (f) that accepts a type Y and returns a type Z and another function (g) that accepts a type X and a type Y that returns another function that accepts a type X and returns a type Z. We express this as f o g.

Here is an example in the favourite programming language of some:

    
~~~{.Java}
class Foo {
  static String f(int i) {
    return Integer.toString(i);
  }

  static int g(char c) {
    return c + 7;
  }

  static String composeFG(char c) {
    return f(g(c));
  }
}
~~~


Easy Peasey right? In my statement above, the type X is `char`, Y is `int` and Z is `String`. Function composition is written in Java as `f(g(a))`. Back to the definition of idempotence, which states (in Java syntax), that f(f(a)) is equivalent to f(a). Clearly we see that f **must** at least have the same type in its argument and return type in order to satisfy idempotence. This means that our previous example has no idempotent functions, however, they were all referentially transparent. Furthermore, a function that is referentially transparent and having the same type in its argument and return type is _not necessarily_ idempotent. A counter-example is easily produced:


    
~~~{.Java}
class Bar {
  static int add1(int i) {
    return i + 1;
  }
}
~~~


Since there exists at least one (in fact, all in this case) arg for `add1(add1(arg))` such that it is not equivalent to `add1(arg)`, then `add1` is **not** idempotent. For example, `add1(add1(7))` is not equivalent to `add1(7)`, clearly.

Here are some insights. Adhering with my naming convention, suppose the function `add0`. Guess what? It **is** idempotent. Also, `multiply1` is idempotent. Every single function on this web page is referentially transparent, but only the two aforementioned hypothetical functions are idempotent.

We might write this using logic as follows for `add0`


> 
∀ n | n ∈ Z. n + 0 = n



For all n such that n is an element of the set of integers, n + 0 = n [is a true statement].

...and for `multiply1`


> 
∀ n | n ∈ Z. n * 1 = n



For all n such that n is an element of the set of integers, n * 1 = n [is a true statement].

For completeness, here is an actual idempotent function with the added twist of a polymorphic type parameter (generic parameter in Java speak):


    
~~~{.Java}
class Baz {
  static <t> T identity(T t) {
    return t;
  }
}
~~~



I hope this clears some things up :)

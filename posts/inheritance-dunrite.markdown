---
comments: true
date: 2007-07-21 19:27:04
layout: post
slug: inheritance-dunrite
title: Inheritance dunrite
wordpressid: 35
tags: Programming
---

I am absolutely surprised at the number of failed examples of inheritance. What troubles me is the extent of ignorance prevailing on the subject of inheritance. [Every once in a while, I read an old rant by a Blub programmer making claims to an understanding of a topic while simultaneously proclaiming that there are so many others without this understanding.](http://www.manageability.org/blog/stuff/inheritance_revisited/) Less often, I am compelled to write something myself.

I will start by using Blub grammar and syntax, but try to keep the usual euphemisms and loaded words to a minimum (I have two, annotated with warnings).

Look at the following interface declarations:

    
~~~{.Java}
interface I {
  T i();
}

interface J {
  U j();
}

interface K extends I, J {}
~~~



If a method accepts a type K, what that method **really** means is that it accepts a type that is composed of both type I and J, **which is more general than type K**. I am ignoring the fact that interface K may have some additional restrictions that are not enforced by the type system, because well, this is Blub after all and using type systems to denote formal requirement specifications is well beyond Blub. If you have a type L that is composed of I and J, you will not be able to pass it to the method. Instead, you need to write an "Adapter" (WARNING: euphemism) from L to K.

I now propose a hypothetical language (still Blub though) that allows you to pass a "`I with J`", which means it might be a K or it might be some other composition of I and J. Note that a compiler can not deduce that K is composed of I and J and generalise this type, since this requires the compiler to compile I, J, K and all compositions of I and J, which is potentially infinite. In other words, time goes this way --->, not that way <--, despite what the Blub type systems say.

In my hypothetical language, you do not explicitly give method arguments types. Instead, if you call both `i()` and `j()` within the aforementioned method, the compiler or _type inferencer_ will determine that the method argument has type `I with J`. Surely, you have seen software in Blub languages, where you pass a type with a bazillion methods and only one or two of those methods are used when it is passed? It's a problem, right? Ever used one of those "mock frameworks"? (WARNING: euphemism for "workaround"). Have you ever passed a K and only called `i()`?

In my hypothetical language, if you only call `i()`, then the method argument type will be inferred to I, not K, not even `I with J`, since remember, we do not explicitly annotate types on method arguments.

Now, is my hypothetical language really hypothetical? No! You see, there are (at least) not-so-crap type systems out there that permit this level of flexibility. They are far more powerful than this little rant purports, but you'll just have to take my word for that. What I have effectively described is (one aspect of) [Type Classes from a language called Haskell](http://en.wikibooks.org/wiki/Haskell/YAHT/Type_basics), which can also [be used in Scala](http://blog.tmorris.net/the-power-of-type-classes-with-scala-implicit-defs/) and other languages with at-least-reasonable type systems.

In other words, _inheritance dunrite_ is as easy as putting down those languages that have got it all wrong and stop pretending that they are sensible or "industry grade". Stop trying to understand their inherent contradictions through cognitive dissonance, write books about the nonsense, invent design patterns, refactorings, workarounds and mock this and that and framework foo and blah blah blah (can I breath now?). Furthermore, standing on a pedestal and proclaiming that "I know something that you don't so na na!" is really quite silly, even if it is true (which it isn't more often than not as has been seen!).

There is much more to inheritance than even the most expert Blub programmer can possibly imagine. I have only just touched the surface by introducing type classes here.

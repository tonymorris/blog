---
comments: true
date: 2007-09-13 10:27:00
layout: post
slug: type-safe-scala-sequence-comprehensions
title: Type-safe Scala sequence comprehensions
wordpressid: 45
tags: Programming
---

On page 85 of [Scala By Example](http://www.scala-lang.org/docu/index.html), the last paragraph states the following in reference to generalising sequence (aka for) comprehensions:


> 
It would be attractive to enforce these types statically in the Scala compiler, for instance by requiring that any type supporting for-comprehensions implements a standard trait with these methods 1 . The problem is that such a standard trait would have to abstract over the identity of the class C, for instance by taking C as a type parameter. Note that this parameter would be a type constructor, which gets applied to several different types in the signatures of methods map and flatMap. Unfortunately, the Scala type system is too weak to express this construct, since it can handle only type parameters which are fully applied types.




This type system feature being described is called _higher-kinded types_. Sadly, none of the mainstream, statically-typed languages offer this feature, which leads to enormous amounts of repetition.

In fact, many of the arguments in favour of dynamic languages that I have seen, are refuted simply with the introduction of this very key, but relatively simple (to other type system features) feature. In other words, I only have to batter an eyelid before I have refuted those arguments, which brings me great suspicion of any underlying plausibility. Perhaps there are other arguments that are more plausible, but I have yet to hear any. I digress...

Higher-kinded types would be available in Java/C# if you could write something like this:

    
~~~{.Java}
static <C<_>, A, B> C<B> map(Convert<A, B> c, C<A> container) {
  ...
~~~



Again, sadly, you cannot. But in Scala you can! In other words, the paragraph quoted above is, at least from how I interpret it, false. However, do understand that at the time the paragraph was written, it may have been true (I can neither confirm nor deny this), but my reckoning is that this piece of documentation needs updating at the least.

Without further ado, I introduce the my refutation:

    
~~~{.Scala}
trait Functor[f[_], a] {
  def map[b](f: a => b)(ft: => f[a]): f[b]
}

trait Monad[m[_], a] {
  def flatMap[b](ma: => m[a])(f: a => m[b]): m[b]
}

trait Filter[c[_], a] {
  def filter(f: a => Boolean)(c: => c[a]): c[a]
}
~~~



My assertion now, is that a Scala sequence comprehension could indeed be written to be type-safe if it only accepted types that adhered to the above interfaces. Am I wrong and if so, what have I misunderstood?

Here are some example (incomplete) implementations of `foreach`:

    
    
~~~{.Scala}
object Comprehension {
  type Comprehension[c[_], a] = Functor[c[a], a] with Monad[c[a], a] with Filter[c[a], a]
  def foreach[c[_], a](c: c[a], e: a => Unit, p: a => Boolean)(implicit ca: Comprehension[c[a], a]): Unit = ca.map(a => if(p(a)) e(a))(c)
  def foreach[c[_], a](c: c[a], e: a => Unit)(implicit ca: Comprehension[c[a], a]): Unit = foreach[c[a], a](c, e, (a: a) => true)
  def foreach[c[_], a, b](c: c[a], f: a => b, p: a => Boolean)(implicit ca: Comprehension[c[a], a]): c[b] = ca.map(a => f(a))(ca.filter(p)(c))
  def foreach[c[_], a, b](c: c[a], f: a => b)(implicit ca: Comprehension[c[a], a]): c[b] = ca.map(a => f(a))(c)
}
~~~



I await an explanation for what I think is the unwarranted undermining of Scala's value by Scala's own documentation!

---
comments: true
date: 2008-07-05 09:48:06
layout: post
slug: implicits-for-the-fearless
title: Implicits for the Fearless
wordpressid: 94
tags: Programming
---

Some programmers are married to the imperative, side-effecting mindset. This makes them fearful of Scala's `implicit` keyword (among many other high-level programming constructs and abstractions). You can read all sorts of amateurish criticism of this language construct on various websites, but I plan to show why they are a necessity to the language being useful (in the intellectually true and meaningful sense -- not in a "Java" "pragmatist" sense (I know I'm treading on thin ice here, but the point is worthwhile)).

I also do not plan to address the amateurish claims specifically, since if you are like me, you simply acknowledge their existence, feel a bit sad, then get over it and move on. I mention these cases so that they can be contrasted to my plan to address a discussion I had with someone (who shall remain nameless for now and leave it up to them if they wish to reveal themselves) who put forward an _intellectually constructed_ argument about why they can/should be eliminated. These kind of discussions are very purposeful. While my opponent's argument was well thought-out (in contrast to the aforementioned), I still think it is wrong. Here is why.

You can do away with `implicit` definitions in favour of named functions with partial application. This is true, but is it _useful_? Furthermore, Scala is not exactly friendly to partially applying function arguments, requiring varying levels of awkwardness in certain contexts. I will assume, since I am a nice guy, that this is not the case. I will now alter the argument to what I consider to be equivalent, "Haskell can do away with type-classes". I altered the argument simply for the sake of brevity in discussion -- I will tie it back to Scala code examples at the end.

Let us assume we get rid of just one Haskell type-class, `Monad`. A monad is a _useful_ abstraction, so we would certainly have to represent it (if you don't believe this, then it is almost certain that [you have reinvented monads inadvertently](http://sigfpe.blogspot.com/2006/08/you-could-have-invented-monads-and.html)). We might do this as follows:

    
~~~{.Haskell}
{-# LANGUAGE RankNTypes #-}

data Monad' m = Monad' {
  unit :: (forall a. a -> m a),
  bind :: (forall a b. m a -> (a -> m b) -> m b)
}
~~~



Then, when it comes to instances, we would write:


    
~~~{.Haskell}
listMonad = Monad' (return :: a -> [a]) (>>=)
maybeMonad = Monad' (return :: a -> Maybe a) (>>=)
~~~



Now when we wish to write the all-important functions over monads, we have to do the following:


    
~~~{.Haskell}
sequence' monad [] = unit monad []
sequence' monad (x:xs) = let (>>>=) = bind monad in x >>>= \y -> sequence' monad xs >>>= \ys -> unit monad (y:ys)

mapM' monad f as = ... etc. etc.
~~~



The proposal by my opponent in the discussion is that we now write a version of each monad function for each instance by partially applying the monad instance. This is very cumbersome and importantly, _less useful_.


    
~~~{.Haskell}
listSequence = sequence' listMonad
maybeSequence = sequence' maybeMonad

listMapM = mapM' listMonad
~~~
    ... and so on.
    



This creates what would otherwise be code in the order of O(1) to turn into O(n) code! This is not good. Do we get any benefit? No, there is no benefit, other than, in the case of Scala, appeasement of 'the irrational squad' i.e. those fervently offended by the suggestion to divorce imperative programming. Let us just address this irrationality, even if just briefly, so we can move on.

Unlike Scala, Haskell's side-effects are controlled, so the mind-shift is _forced_ by the compiler (a successful form of counselling?). A Scala implicit function _can_ perform side-effects. This would be very bad; if they were controlled, would these irrational objections be raised? The answer is no and it is quite easy to expose.

A claim to the contrary is often a case of [doublethink](http://en.wikipedia.org/wiki/Doublethink). Java is a programming language that deliberately encourages side-effects and by implication (also, by its poor type system), makes abstraction and composition extremely difficult to the point of impossibility very early on (see [Functional Java](http://functionaljava.org/) for evidence). Yet even in this language -- perhaps the most contrived available -- we find implicit conversions that attract no objections whatsoever!

Imagine this Java signature `int toInt(char c)`. Such a user-defined function could indeed perform side-effects and any language feature permitting implicit use of this function is likely to attract criticism. Yet, this is one example of such a function built right-in to the Java language, which is _guaranteed by the language specification_ to perform no side-effects. It is the fact that it is internalised by the user to perform no side-effects that makes it acceptable.

This is valid Java:

    
~~~{.Java}
int i = c; // where c is of type char
~~~



I don't wish to labour this point, other than to make it clear how easy it is to crush these irrationalities (it continues on in a usual uninteresting fashion).

Requiring code to blow out to O(n) from O(1) is itself not bad, but to further observe that there is no gain from it is conclusive. Interestingly, this is similar to a complaint by David MacIver (and myself -- I am just less vocal about it) that Scala lacks default function argument lists in preference for overloading. This also creates unnecessary code-bloat (O(1) -> O(n)) for no benefit. (I hope I am not misrepresenting David here -- I have lost the reference to his complaint).

Here is the equivalent Scala code to the above, however, Scala's type system lacks the ability to pass the `unit` and `bind` functions -- instead requiring the use of traits.


    
~~~{.Scala}
trait Unit[U[_]] {
  def unit[A](a: A): U[A]
}

trait Bind[M[_]] {
  def bind[A, B](ma: M[A], f: A => M[B]): M[B]
}

case class Monad[M[_]](u: Unit[M], b: Bind[M])

object Monad {
  val listMonad = Monad[List](new Unit[List] {
    def unit[A](a: A) = List(a) }, new Bind[List] {
    def bind[A, B](ma: List[A], f: A => List[B]) = ma flatMap f
  })

  def sequence[M[_], A](m: Monad[M], as: List[M[A]]): M[List[A]] = as match {
    case Nil => m.u.unit(Nil)
    case x :: xs => m.b.bind(x, (y: A) => m.b.bind(sequence[M, A](m, xs), (ys: List[A]) => m.u.unit(y::ys)))
  }

  def listSequence[A](as: List[List[A]]) = sequence[List, A](listMonad, as)
  def optionSequence[A](as: List[List[A]]) = sequence[Option, A](optionMonad, as)
  // etc. etc. O(n)!
}
~~~



As you can see, I was being _very_ nice when I converted the argument to Haskell. The argument against such a proposal becomes even stronger in the context of Scala.

To my opponent, thanks for the discussion; it was fun and the intellectual maturity is admirable :)

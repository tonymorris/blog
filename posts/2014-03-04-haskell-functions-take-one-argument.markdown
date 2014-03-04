---
comments: true
date: 2014-03-04 08:00:00
layout: post
slug: haskell-functions-take-one-argument
title: Haskell Functions Take One Argument
tags: Haskell, Function, Programming, Functional Programming, Teaching
---

I teach functional programming. It is a significant part of my job. I enjoy it a
lot and it is extremely challenging; in my opinion, much more so than learning
functional programming itself.

I use the Haskell programming language for teaching functional programming. I
like to emphasise the construction of *concepts* over any specific programming
language. I am not into teaching programming languages; I really find that
boring, uneventful and unhelpful for all involved. However, learning some of the
intricacies of Haskell itself is inevitable. It doesn't take long though and is
very much worth the investment of effort if you aspire to learning concepts.

That is to say, using (almost all) other programming languages for this
objective is a false economy that is not even a close call. One could spent many
weeks or even years demanding to articulate concepts in a specific programming
language, only to struggle precisely because that language resists an accurate
expression and formation of that concept. I have seen this an overwhelming
number of times. It is often supported by the fallacy of false compromise, "but
all languages are Turing-complete, so I am going to use JavaScript, which even
has first-class functions, in order to come to understand what monad means."

No, you won't, I absolutely insist and promise.

This is all somewhat beside the point of this post though. The point is that
there is a fact, which often comes up in teaching, that can be expressed briefly
and concisely. It requires no exceptions, apologies or approximations. I would
like to state this fact and explain some of the nomenclature that surrounds this
fact.

> All functions in the Haskell programming language take exactly one argument.

This fact is certain to come up early on in teaching. If a student comes to
trust then follow this fact, then progress will be unhindered. That is because
it is an absolute fact. However, even though a student may initially convince
themselves of this fact, it has been my experience that they will renege on it
at some time in the future.

The use of casual terminology such as the following surely helps to set this
trap:

> Examining the signature to the `map` function, we see that it takes *two
arguments*:

~~~{.Haskell}
map :: (a -> b) -> List a -> List b
~~~

We will then talk about the *first argument* and the *second argument* as if
there even is such a thing.

However, the truth of the original fact has not changed. Look at it, just
sitting there, saying nothing, being all shiny and true. So how could all
functions take one argument, while we simultaneously and casually talk about a
"second argument"? Are we just telling great big lies? Have we made a mistake?

The problem is our vocabulary. In our spoken words, we are using an
**approximation** of fact and superficially, it looks like a blatant
contradiction. Let us expand our approximation to more obviously coincide with
our statement of fact. I have added some annotation in [brackets].

> Examining the signature to the `map` function, we see that it is a function
[therefore, it definitely takes one argument]. That argument is of the type
`(a -> b)` [which is also a function]. The return type of the `map` function is
`(List a -> List b)` which is a function and [since it is a function] takes one
argument. That argument is of the type `(List a)` and it returns a value
[not a function]. That value is of the type `List b`.

When we say out loud "the `map` function takes two arguments", we are
approximating for the above expansion. It is important to understand what we
really mean here.

During my teaching, I will often make a deal with students; I will use the
terser vocabulary with you and I will even let you use it, however, if at any
moment you violate our understanding of its proper meaning, I will rip it out
from under you and demand that you use the full expansion. Almost always, the
student will agree to this deal.

Some time after having made this deal, I will hear the following question.
Given, for example, this solution to an exercise:

~~~{.Haskell}
flatten ::
  List (List a)
  -> List a
flatten =
  foldRight (++) Nil
~~~

I will hear this question:

> Wait a minute, you only passed two arguments to `foldRight`, however, we have
seen that it takes three. How could this possibly work!?

Here is another example of the question. Given this solution:

~~~{.Haskell}
filter ::
  (a -> Bool)
  -> List a
  -> List a
filter f =
  foldRight (\a -> if f a then (a:.) else id) Nil
~~~

I will hear this question:

> The argument to `foldRight` (which is itself a function) takes two arguments,
however, the function you passed to `foldRight` has been specified to take only
one (called `a`). How could this even work?

It is at this moment that I hand out an infringement notice under our agreed
penal code for the offence of:

    Section 1.1
    Failure to Accept that All Haskell Functions Take One Argument

    Penalty
    Temporary Incarceration at Square One with release at the discretion of an
    appointed Probation Officer

I understand that in a learning environment, it may be easy to demonstrate and
subsequently accept this fact, then later fall afoul when previously learned
facts interfere with this most recent one. The purpose of going back to square
one and starting again is to properly internalise this fact. It is an important
one, not just for the Haskell programming language, but for Functional
Programming in general.

Joking aside, the purpose of this post is to help reconcile these observations.
There is a recipe to disentanglement. If you find yourself in this situation,
follow these steps:

1. Revert back to the fact of matter; all Haskell functions always take one
   argument. There is never an exception to this rule (so you cannot possibly be
   observing one!).
2. From this starting position, reason about your observations with this fact in
   mind, even if it is a little clumsy at first. After some repetitions, this
   clumsiness will disappear. Persevere with it for now.
3. Introspect on the thought process that led you into that trap to begin with.
   This will help you in the future as you begin to trust that principled
   thinking will resolve these kinds of conflicts. It can be initially clumsy,
   even to the point of resisting on that basis, but that is a one-time penalty
   which quickly speeds up.

Hope this helps.

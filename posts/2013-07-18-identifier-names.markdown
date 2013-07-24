---
comments: true
date: 2013-07-18 20:00:00
layout: post
slug: identifier-names
title: Sticks, stones, but names are not useful to me
tags: Programming, Scala, Parametricity, Identifiers, Code Comprehension, Bayes' Theorem, Theorems for Free
---

There are various tools for comprehending code. It is often alleged to me that identifier names are one such tool — an
important one even. In order to be a useful tool for comprehension, it must have a degree of efficacy above an arbitrary
hypothesis. For example, suppose the following code:

~~~{.Scala}
def reverse: List[Banana] => List[Banana]
~~~

Without viewing the body of the code, one might infer that this code "reverses the list." What does it mean to reverse a
list? Let us try to rigorously define reversing a list:

* Reversing the empty list produces the empty list.
* Reversing a single-element list produces that same list.
* Taking two lists, appending them then reversing, produces the same list as reversing each, then appending
  the latter to the former.

We infer all these things, even if informally, when we conclude that this function reverses the list. We might have
some degree of confidence in concluding that all these properties hold purely from the function name, however, as part
of this conclusion, we also conclude that the function also does not peel the first `Banana`. We have no evidence of
these facts, except for the function name, alleged to be useful to infer some confidence about these facts.

In order for this method of comprehension to be efficacious, it must produce a result better than guessing. That is to
say, the degree of confidence invoked by inferring that "this function reverses the list" from the premise, "because the
function is named `reverse`", must be higher than inferring that the function does not reverse the list, from the same
premise. In my experience, with which some will certainly disagree, this is not the case, rendering this comprehension
method useless. That is to say, the identifier name persuades confidence of the inference by nil, not even a bit.

Thankfully, this is unimportant. It is unimportant because there exist methods of code comprehension that are
*significantly more effective*, so you can abandon the question of whether or not there is efficacy of using identifier
names for inferring properties of code.

----

Suppose the following code:

~~~{.Scala}
def reverse[A]: List[A] => List[A]
~~~

At first glance, it might appear that we must take a leap of confidence in inferring that the function reverses the list. However, we can infer the following fact — *this function definitely does not peel the first `Banana` in the list*. I
can infer this because if the function attempted to do this, it *would not compile*[^1]. In fact, I can infer lots of
things here, such as:

* The `reverse` function did not add `10` to every second list element.
* All elements in the list returned by `reverse` are contained in the input list.

We are able to infer these things simply by making the `reverse` function *parametric*. We are no longer reversing a
list of bananas — although that might be our use-case — we are reversing a list of `A` for all values of `A`. One virtue
of this *parametricity* is that we can infer a significant number of things that *do not occur*. This theme of learning
what does not occur is ubiquitous when deploying these programming techniques and is described in more detail by
Wadler[^2].

Here is another example, using a programming language called SafeHaskell (very similar to Haskell):

~~~{.Haskell}
add10 :: Int -> Int
~~~

By the name of the function, we might unreliably infer (OK, let's be honest, we are making a bold guess) that the
function adds `10` to its argument. However, looking at the type, we know for sure that the function *did not print its
argument to the standard output stream*. We know this because had the library provider attempted it, the code would not
have compiled. To be clear, it would not be a valid SafeHaskell program, so our assumption that we are looking at
SafeHaskell fails, forcing us to unify by selecting one of the following:

* The function does not print its argument to the standard output stream.
* We are not looking at SafeHaskell source code.
* We are using an escape hatch, implied by the halting problem.

There are simply no other options. What other things can we reliably conclude this function does not do?

Here is yet another example:

~~~{.Scala}
def constant[A, B]: A => B => A
~~~

In this case, we can reliably infer that this function does one single thing. It ignores its second argument and returns
its first. This might be protested:

* It might perform a side-effect first!
    - This is true, but assuming a pure subset of the language is useful for reasons other than this one.
* It might type-cast or type-case (`asInstanceOf` or `isInstanceOf`).
    - This is another unfortunate escape hatch in the Scala type system that conveniently permits unsafe code.
* It might recurse forever, return `null` or throw an exception.
    - This is yet another escape hatch.

So why dismiss these protests? They are inescapable implications of the halting problem. The more practical question is,
"how convenient does Scala make these escape hatches available?" and the answer is an unfortunate one — it can often
appear to be easier to exploit these escape hatches, but it won't be too long before the penalty is paid. Although in
practice, it is easier both short and long term to avoid these escape hatches, the illusion of convenience persists in
some cases.

If we are to concede these abilities, it is simply up to our own discipline to enforce that we have not attempted to
take the illusory easy way out. This includes using a pure-functional subset of Scala, which is a lot easier than is often made
out. For example, the `scalaz.effects.STRef` data type permits a pure-functional `var` that has all the capabilities of
`var` while also maintaining all the aforementioned abilities (unlike `var` itself). This is a win-win. [Rúnar Bjarnason goes into detail on this at QCon 2013.](https://qconnewyork.com/sites/default/files/QConNY2013_RunarBjarnesson_PurelyFunctionalIO.pdf)

By these methods of exploiting the type system, we are able to very reliably infer things that did not occur and
occasionally, infer and conclude the only thing that did occur. However, what about narrowing it down further? We know
that the (parametric) `reverse` function doesn't manipulate the list elements, but how do we know it reverses the list?
Do we fall back to relying on the function name and simply hope so?

No.

We continue to use more reliable methods of code comprehension. Let us restate the definition of `reverse`, however, we
can will include Scala source code. All expressions must return `true` regardless of the value of any of their
arguments:

* Reversing the empty list produces the empty list
    * `reverse(Nil) == Nil`
* Reversing a single-element list produces that same list
    * `element => reverse(List(element)) == List(element)`
* Taking two lists, `l1` and `l2`, appending them then reversing, produces the same list as reversing each, then
  appending the reverse of `l1` to the reverse of `l2`.
    * `(l1, l2) => reverse(l1 ::: l2) == (reverse(l2) ::: reverse(l1))`

If we can be confident that these properties hold, then we can also be confident that our `reverse` function does in
fact, reverse the list. In fact, there is no other possible function that satisfies the type and these properties,
besides the one that reverses a list. Again, we have not resorted to the function name for code comprehension — we have
inspected *algebraic properties about the code*.

So how do we increase confidence that these properties hold?

Unfortunately, an implication of the halting problem is that we cannot prove these program properties, for the general
case. This is not the end of the world though — we can still attempt to *disprove* these program properties. That is to
say, we can go to efforts to determine if the function is *not* one which reverses the list. We can express our
algebraic properties, which give away the full specification of the function, then automate the assertion that there
exist values for which the property does not hold. This automation is precisely what [ScalaCheck](http://code.google.com/p/scalacheck/) does, however, the expression itself is enough to rigorously specify the function behaviour without degenerating to faith in function names.

The next time you see a function named `<-:` and you think to yourself, "OMG, how unreadable, what am I going to do
now!?", ask yourself if there are other tools — perhaps more robust than those familiar — to comprehend the code.
What is its type? What are its algebraic properties? Are there parametric properties to exploit?

What if it has this type?

~~~{.Scala}
def <-:[A, B](f: A => B): List[A] => List[B]
~~~

Does it map the function across the list elements? Maybe. However, we definitely know that the elements in the resulting
list came from running the given function on an element from the input list. You see? Parametricity, just like that, was
73 fuck-loads more reliable than looking at the function name to comprehend how this code works, and this is only the
start of answering the question. We have many more tools at our disposal. Importantly, they are _reliable_. I like
reliable, because I also like things that are true. Hopefully you do too!

----

So what about these allegations of utility of identifier names? Do they have any merit at all?

No. Insistence on the value of identifier names for code comprehension has some glaring holes in it. Let us put aside
that there are far more robust methods of comprehension. Let us also put aside that the claims are probably motivated by
a lack of familiarity with superior tools.

Here is why this allegation is not just bullshit, but very obviously bullshit. Anyone who names a data type,
`AbstractAdvisorAutoProxyCreator` is just as committed to not using identifier names for meaning as anyone else.
However, there is another level again — the staunch belief that this identifer name is conveying meaning exposes just
how confused that belief is. Any query such as, "What exactly does `AbstractAdvisorAutoProxyCreator` mean?" is met
with handwaving. This is because __nobody knows what `AbstractAdvisorAutoProxyCreator` means__ and the only practical
implication here, in the world in which we all find ourselves, is one or more scatterbrains holding a belief otherwise.

From a näive perspective, this situation appears to be a ripe learning opportunity. There appears to be a lot to be
gained simply by sharing knowledge with a beginner — a trivial investment of effort. So why not take it? That question
is fraught with complexity, but often, it is more constructive to have a giggle, a little lament, then dismiss the
confused allegations.


[^1]: Due to the halting problem, there are some additional premises required to further increase confidence in this
conclusion.

[^2]: Wadler, Philip. "Theorems for free!." Proceedings of the fourth international conference on Functional programming languages and computer architecture. ACM, 1989.

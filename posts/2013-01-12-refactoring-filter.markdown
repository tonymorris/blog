---
comments: true
date: 2013-01-12 19:00.00
layout: post
slug: refactoring-filter
title: Refactoring filter
tags: Programming
---

## The `filter` function

The `filter` function is one that accepts a list as an argument, keeping all elements that satisfy a given predicate, discarding all others and returns the resulting list. For example, I might use the `filter` function in Haskell to keep all **even** elements in a list:

~~~{.Haskell}
λ> filter even [2,3,6,2,4,7,6,75,22]
[2,6,2,4,6,22]
~~~

or perhaps using Scala:

~~~{.Scala}
scala> List(2,3,6,2,4,7,6,75,22) filter (_ % 2 == 0)
res0: List[Int] = List(2, 6, 2, 4, 6, 22)
~~~

In both cases, the list structure under examination is a [cons list](http://en.wikipedia.org/wiki/Cons).

When I teach functional programming, we are usually implementing the `filter` function on [our own cons list using Haskell](https://github.com/tonymorris/course/blob/master/src/L02/List.hs#L80)[^1] on the first day. Having done this enough times now, with both Haskell and Scala, I observe a recurring pattern.

The usual solutions are given either with explicit pattern-matching along with explicit recursion:

~~~{.Haskell}
fiilter _ [] = []
fiilter p (h:t) = if p h then h:fiilter p t else fiilter p t
~~~

…or by using the provided `foldRight` function, which abstracts the pattern-matching and recursion for cons lists:

~~~{.Haskell}
fiilter p x = foldRight (\a b -> if p a then a:b else b) [] x
~~~

## Re-implementing `filter` as an exercise

Whichever solution is given, they are both correct. However, almost invariably, at this stage in the exercise, there is a developing suspicion that this solution can be *done better*. I am usually quizzed about how to improve the solution. For example, in the pattern-matching solution, both sides of the `if` branch repeat the code `fiilter p t`. Indeed, even in the `foldRight` solution, there is application to the value `b` on both sides of the `if` branch -- a classic example of repetitive code (we functional programmers take the DRY principle seriously, even obsess over it!).

So let's get on with the refactor. Since we are using pure functional programming, we are able to deploy *equational reasoning*. In other words, we are able to shuffle our expressions around, so long as we have an equivalent expression, because we are void of side-effects.

## Denying equational reasoning

Let's take a quick diversion for a minute and imagine if we did not have this guarantee and suppose we were to tidy up this code:

~~~{.Haskell}
if p then f x else f y
~~~

This code can be refactored to remove the repetition of application of the function `f`:

~~~{.Haskell}
f (if p then x else y)
~~~

However, imagine if `f` performed a side-effect! For example, if `f` printed its argument to standard output, then our refactoring would have altered the way the program runs.

Thankfully, in our context, we are guaranteed that this is not the case, so we can go right ahead.

## Refactoring `fiilter`

I am going to demonstrate a refactoring of `fiilter` at every intermediate step. Some people skip these steps when they do this for themselves, but I will specify each step that I usually see. The first step in refactoring the pattern-matching solution is to use `foldRight` as has already been given:

~~~{.Haskell}
fiilter p x = foldRight (\a b -> if p a then a:b else b) [] x
~~~

Once we get to this step (or perhaps we started here), we move on to removing the repetition in the lambda expression -- specifically, the application to the value `b` appearing on each branch of the `if`.

First, let's introduce the `id` function, perhaps the easiest function to understand ever!

~~~{.Haskell}
id :: a -> a
id a = a
~~~

That's right, the `id` function takes one argument and simply returns it. That means I can refactor the above code to this, without changing the way the function behaves:

~~~{.Haskell}
fiilter p x = foldRight (\a b -> if p a then a:b else id b) [] x
~~~

All I have done here is use `id` on one side of the `if` branch. Next I am going to do a purely syntactic transformation. I am going to move the use of `(:)` into prefix position. I am doing this to help make the next step a bit more obvious :)

~~~{.Haskell}
fiilter p x = foldRight (\a b -> if p a then (:) a b else id b) [] x
~~~

Now we apply the true side to `b` using `(:) a` and the false side using `id`. We can refactor that away by returning a function on each branch, then applying that result to `b`:

~~~{.Haskell}
fiilter p x = foldRight (\a b -> (if p a then ((:) a) else id) b) [] x
~~~

Great! The application to the value `b` occurs only once. Next we can remove the explicit value `b` that is declared for the lambda expression, then we merely apply a function to it. Let's just return that function! In other words, since this:

~~~{.Haskell}
\x -> f x
~~~

…can be replaced with this:

~~~{.Haskell}
f
~~~

…and this:

~~~{.Haskell}
\x y -> f x y
~~~

…can be replaced with this:

~~~{.Haskell}
\x -> f x
~~~

…then this:

~~~{.Haskell}
fiilter p x = foldRight (\a b -> (if p a then ((:) a) else id) b) [] x
~~~

…can be replaced with:

~~~{.Haskell}
fiilter p = foldRight (\a -> if p a then ((:) a) else id) []
~~~

OK, this is great! We are simply replacing each cons cell with the given function[^2] and we needn't specify the list in the argument list only to apply to it on the far right. But are we finished?

## Final housekeeping

Unfortunately, Haskell (and most languages) provide `if` as syntax. Since Haskell is a non-strict language, we can easily write it ourselves as a library function:

~~~{.Haskell}
if' :: Bool -> x -> x -> x
if' p t f = if p then t else f
~~~

So how does out code look?

~~~{.Haskell}
fiilter p = foldRight (\a -> if' (p a) ((:) a) id) []
~~~

No real improvement.

However, for consistency with other *catamorphisms*[^3] in the Haskell library, we should shuffle the argument order[^4]. I will also move the false branching argument to the *first position*. There is an underlying theoretical reason for this, but run with me for now!

~~~{.Haskell}
if' :: x -> x -> Bool -> x
if' f t p = if p then t else f
~~~

Great, we have a branching function with arguments in appropriate order :)

OK so what about now?

~~~{.Haskell}
fiilter p = foldRight (\a -> if' id ((:) a) (p a)) []
~~~

Still no real difference :( However, there is a very regular pattern to be observed here, perhaps a little too advanced for day one, but it is here nonetheless!

### Function composition

Let's first do a small refactor on our expression using function composition. Any expression of the form:

~~~{.Haskell}
\x -> f (g x)
~~~

can be written using function composition:

~~~{.Haskell}
f . g
~~~

Therefore, our expression `\a -> if' id ((:) a) (p a)` can be written:

~~~{.Haskell}
\a -> ((if' id . (:)) a) (p a)
~~~

Although, this is a somewhat messy step, it allows us to get to the next one, where the purpose becomes a bit more apparent.

### The SK(I) combinator calculus

We have an expression of this form:

~~~{.Haskell}
\x -> f x (g x)
~~~

This is known as *the S combinator of [the SK(I) combinator calculus](http://en.wikipedia.org/wiki/SKI_combinator_calculus)*[^5]. The S combinator generalises to an applicative functor[^6] and we exploit this to take our refactoring further. The S combinator (generalised) in Haskell is written with `(<*>)`[^7] and you will need to `import Control.Applicative` to use it.

The expression `\x -> f x (g x)` can be written `f <*> g` and we have an expression of precisely this form:

~~~{.Haskell}
\a -> ((if' id . (:)) a) (p a)
~~~

which can be refactored to:

~~~{.Haskell}
if' id . (:) <*> p
~~~

This is great! So now our solution for `fiilter` looks like this:

~~~{.Haskell}
fiilter p = foldRight (if' id . (:) <*> p) []
~~~

## Finally

This is how I might express a final solution. Taking the refactoring this far is questionable to begin with, however, it is at least a great exercise in the application of equational reasoning and pattern recognition.

The readability of this code might also be questioned, however, it is very important to properly follow the process of pattern recognition before developing opinions about readability. I usually cannot address this concern quickly or in a helpful way. so I smile at this point and move on to the next exercise. Let's do that :)

[^1]: The function names have been altered to prevent clashing with those built-in.

[^2]: I emphasise and develop a very specific intuition of `foldRight` (and `foldLeft`) before attempting this exercise, so I be sure to use terminology that is specific to this explanation.

[^3]: `foldRight`, `either`, `maybe` are all functions on which the final argument is the structure that is being *folded*. There is a very good reason for this, but let's get on with it!

[^4]: Since `if` is the fold (catamorphism) for the `Bool` structure, we will move the `Bool` to the final argument position.

[^5]: Note the definition on the wiki, `Sxyz = xz(yz)` or in haskell syntax, `s x y z = x z (y z)`{.Haskell}.

[^6]: The `((->) t)` applicative functor is the S combinator.

[^7]: I am often asked if this has a pronunciation and although I don't know of an agreed term, I have heard: "spaceship operator", "starship operator", "angle bum."
---
comments: true
date: 2013-10-18 20:00:00
layout: post
slug: applicative-do
title: Improving Applicative do-notation
tags: Programming, Haskell, Applicative, Monad, Apply, Bind, Functor
---

### Monad and Functor

The [Haskell programming language](http://haskell.org/) has a `Monad` type-class
as well as a `Functor` type-class. It is possible to derive the `Functor`
primitive (`fmap`) from the `Monad` primitives:

~~~{.Haskell}
-- derive fmap from the Monad primitives, (>>=) and return
fmap f x =
  x >>= return . f
~~~

Therefore, it is reasonably argued that `Monad` should extend `Functor` so as to
provide this default definition of `fmap`. Due to history, this is not the case,
which leads to some awkward situations.

For example, since not all `Functor` instances are `Monad` instances, a given
operation may wish to restrict itself (if possible) to `Functor` so that it can
 be used against those data types. In short, use `fmap` instead of `liftM` to
 prevent an unnecessary constraint on the type of the operation.

~~~{.Haskell}
fFlip ::
  Functor f =>
  f (a -> b)
  -> a
  -> f b
fFlip f a =
  fmap ($a) f

mFlip ::
  Monad f =>
  f (a -> b)
  -> a
  -> f b
mFlip f a =
  liftM ($a) f
~~~

The `fFlip` is available to use to a strict superset of the data types that
`mFlip` is available to, yet they are both equivalent in power. It is desirable
to implement `fFlip`. However, when we combine a usage of `fFlip` with a monad
operation, our type constraint becomes `(Monad f, Functor f) =>`, which is
undesirable boilerplate because `Monad` implies `Functor`!

### Monad, Applicative and Functor

A [proposal to amend this introduces the `Applicative` type-class](http://www.haskell.org/haskellwiki/Functor-Applicative-Monad_Proposal), which sits  between
`Monad` and `Functor`. In other words, `Monad` extends `Applicative` and
`Applicative` extends `Functor`. This is again, because the primitives of each
superclass can be derived:

~~~{.Haskell}
-- derive fmap from the Applicative primitives, (<*>) and pure
fmap ::
  Applicative f =>
  (a -> b)
  -> f a
  -> f b
fmap =
  (<*>) . pure

-- derive (<*>) and pure from the Monad primitives, (>>=) and return
(<*>) ::
  Monad f =>
  f (a -> b)
  -> f a
  -> f b
f <*> a =
  do f' <- f
     a' <- a
     return (f' a')

pure ::
  Monad f =>
  a
  -> f a
pure =
  return
~~~

### Applicative do-notation

With this proposal, there is [another proposal to extend do-notation](http://ghc.haskell.org/trac/ghc/wiki/ApplicativeDo)
to take advantage of this improved flexibility. Currently, do-notation
translates to the code that uses the `Monad` primitives, `(>>=)` and
`return`[^1].

There are some arguments against this proposal, because this extension is not
always desirable. In particular, the degree to which values are shared may be
affected. Consider:

~~~{.Haskell}
result =
  do a <- expr
     b <- spaceyExpr
     return (f a b)

-- monad desugaring (current)
result =
  expr >>= \a ->
  spaceyExpr >>= \b ->
  return (f a b)

-- applicative desugaring (proposed)
result =
  fmap f expr <*> spaceyExpr
~~~

Since `spaceyExpr`appears inside a lambda for the current desugaring, it will
not be retained and so computed on each invocation of `a`. However, in the
proposed desugaring, the value is retained and shared when the expression is
evaluated. This could, of course, lead to surprises in space usage.

It might be argued that do-notation should maintain its current desugaring using
`Monad` and introduce another means by which to perform `Applicative`
desugaring.

Whatever the outcome, all of this distracts from the otherwise glaring
oversight.

### No

The Functor, Monad, Applicative proposal opens with the following paragraph:

    Haskell calls a couple of historical accidents its own. While some of them,
    such as the "number classes" hierarchy, can be justified by pragmatism or
    lack of a strictly better suggestion, there is one thing that stands out as,
    well, not that: Applicative not being a superclass of Monad.

It is my opinion that this proposal is about to commit *exactly the same
historical mistake* that is attempting to be eschewed. Furthermore, by
properly eliminating this mistake, the syntax proposal would be improved as a
consequence.

Being a strong proponent of progress, and that Haskell is often pushing the
front of progress, this makes me a bit sad :(

Fact: not all semigroups are monoids.

No desugaring, current or proposed, utilises the identity value. In the
`Monad` case, this is `return` and in the `Applicative` case, this is `pure`.
However, it is a requirement of users to implement these functions. There exist
structures that can utilise the full power of this desugaring, but cannot
provide the identity value. Therefore, we can eliminate the identity value and
still exploit the full advantage of desugaring. Not only this, but it then makes
operations available to a strict superset of data types.

Consider the following amendment to the proposal:

~~~{.Haskell}
class Functor f => Apply f where
  (<*>) ::
    f (a -> b)
    -> f a
    -> f b

class Apply f => Applicative f where
  pure ::
    a
    -> f a
~~~

We may still derive many of the ubiquitous functions, without the full power of
`Applicative`.

~~~{.Haskell}
liftA2 ::
  Apply f =>
  (a -> b -> c)
  -> f a
  -> f b
  -> f c
liftA2 f a b =
  fmap f a <*> b
~~~

We may still exploit our do-notation:

~~~{.Haskell}
result =
  do a <- expr1
     b <- expr2
     return (f a b)

-- apply desugaring
result =
  fmap f expr1 <*> expr2
~~~

However, more to the point, there are now data structures for which these
operations (e.g. `liftA2`) and do-notation become available, that otherwise
would not have been.

Here are some examples of those:

##### Also

~~~{.Haskell}
data NonEmptyList a = NEL a [a]
data Also a x = Also (NonEmptyList a) x
instance Functor (Also a) where
instance Apply (Also a) where
  Also (NEL h t) f <*> Also (NEL h' t') x =
    Also (NEL h (t ++ h' : t')) (f x)
~~~

The `Also` data type has no possible `Applicative` instance, yet it has a very
usable `Apply`. This means we can use (amended) `liftA2` and do-notation on
`Also` values, without losing any power.

This data type generalises in fact, while still maintaining an `Apply` instance.

~~~{.Haskell}
data Also s x = Also s x
~~~

There is an `Apply` instance for `(Also s)` for as long as there is a
`Semigroup` instance for `s`, however, if your semigroup is not a monoid, then
there is no `Monoid` instance. I have used `(NonEmptyList a)` as an example of a
data type with a semigroup, but not a monoid.

~~~{.Haskell}
class Semigroup a where
  (<>) :: -- associative
    a
    -> a
    -> a

instance Semigroup s => Apply (Also s) where
  Also s1 f <*> Also s2 x =
    Also (s1 <> s2) (f x)
~~~

##### OrNot

~~~{.Haskell}
data OrNot a = -- Maybe (NonEmptyList a)
  Not
  | Or (NonEmptyList a)

instance Functor OrNot where

instance Apply OrNot where
  Not <*> _ =
    Not
  Or _ <*> Not =
    Not
  Or (NEL h t) <*> Or (NEL h' t') =
    Or (NEL (h h') (t <*> t'))
~~~

The `OrNot` data is isomorphic to `Maybe (NonEmptyList a)` and has an
`Apply` instance that is similar to the `Applicative` for `Maybe`. However,
since this data type holds a non-empty list, there is no possibility for an
`Applicative` instance.

Again, with an amended do-notation and library functions, we could use `OrNot`
values.

##### But it doesn't stop there…

Your regular old `Data.Map#Map` can provide an `Apply` instance, but not an
`Applicative`.

~~~{.Haskell}
instance Ord k => Apply (Map k) where
  (<*>) =
    Map.intersectionWith ($)
~~~

There is no corresponding `Applicative` instance for this `Apply` instance. This
is the same story for `Data.IntMap#IntMap`.

I want to use `liftA2` and many other generalised functions on `(Map k)` values
and no, I am not sorry!

### Apply not Applicative

I could go on and on with useful data types that have `Apply` instances, but no
corresponding `Applicative`. However, I hope this is enough to illustrate the
point.

If we are going to amend the type-class hierarchy, taking on all the
compatibility issues of doing so, then let us provide a kick-arse solution.
It is especially compelling in that this amendment to the proposal subsumes the
existing error. Let us move on from yet another historical mistake that
[has already been acknowledged](http://hackage.haskell.org/package/semigroupoids).

### Bind not Monad

This story is not just about `Apply` and `Applicative`. All of the same
reasoning applies to semi-monads or the `Bind` type-class. The `return`
operation is not essential to do-notation or even many monad functions, so it is
an unnecessary, imposed requirement for implementers of the `Monad` type-class.

Similarly, there are structures for which there is a `Bind` instance, but not a
`Monad` instance.

### Type-class Hierarchy Proper

In order to take full advantage of a type-class amendment, I submit the
following proposed type-class hierarchy. I contend that it subsumes the existing
proposal by providing additional flexibility for zero additional loss.

Library functions, such as `liftA2`, could slowly adopt an amendment to their
 type signature so as to open up to more data types.

~~~{.Haskell}
class Functor f where
  fmap ::
    (a -> b)
    -> f a
    -> f b

class Functor f => Apply f where
  (<*>) ::
    f (a -> b)
    -> f a
    -> f b

class Apply f => Applicative f where
  pure ::
    a
    -> f a

class Apply f => Bind f where
  (>>=) ::
    (a -> f b)
    -> f a
    -> f b

class (Applicative f, Bind f) => Monad f where
~~~

and while we're at it…

~~~{.Haskell}
class Semigroup a where
  (<>) :: -- mappend
    a
    -> a
    -> a

class Semigroup a => Monoid a where
  mempty ::
    a
~~~

but maybe I am biting off a bit too much there :)

[^1]: There are other functions on `Monad`, but these are either derivable
(e.g. `(>>)`) or a mistake and hindrance to discussion (e.g. `fail`).
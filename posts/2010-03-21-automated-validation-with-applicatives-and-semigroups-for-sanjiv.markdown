---
comments: true
date: 2010-03-21 10:33:36
layout: post
slug: automated-validation-with-applicatives-and-semigroups-for-sanjiv
title: Automated Validation with Applicatives and Semigroups (for Sanjiv)
wordpressid: 668
tags: Programming
---

Sanjiv is an ex-colleague of mine who recently gave an excellent presentation at [Brisbane Functional Programming Group](http://www.meetup.com/Brisbane-Functional-Programming-Group-BFG/). His presentation was on the use of the disjoint union algebraic data type for representing error handling or "validation". This data type is often called `Either`. Sanjiv used [the Scala version](http://www.scala-lang.org/docu/files/api/scala/Either.html) for his presentation.

Here I am going to write a data type that is just like (isomorphic to) `Either` called `Validation`. There will be two constructors for failure and success. I will be using Haskell, Scala and Java. I am going to present a function that is mentioned in an excellent paper called [Applicative Programming with Effects](http://www.soi.city.ac.uk/~ross/papers/Applicative.html).

There is a slight difference in my implementation to the function in this paper. The one mentioned in the paper uses a `Monoid` constraint, while I am using the more general `Semigroup`. This allows data types that are semigroups but not monoids (all monoids are semigroups). In particular, a non-empty list is a semigroup but not a monoid.

This constraint on the failing value allows the user to automatically accumulate errors as they chain through the `Validation` values, for example, by keeping them in a non-empty list. I am not going to give example usages of this code in this post because I think it is a great exercise for others and I do not want to spoil it. It is sufficient to say that any usage that type-checks is likely to be a useful example. I may give examples in the future if there is some confusion that needs clarification.

The function can accumulate `Validation` values using the applicative programming pattern. If all given are successful, you'll be left with a function that takes all the successful values to a new value, which will be returned as a success. If at least one is not successful, the applicative pattern will begin accumulating with that error value and any future values.

I recommend starting with a simple example such as a `Person` type with an `age :: Int` and `name :: String` with imposed validation rules such as "age must be above 0 and less than 130" and "name must start with a capital letter". The applied function at the end should be `ValidatedInt -> ValidatedString -> Person`.

Like I said, if there is any confusion, I'll expand later. Put the questions in the comments. Enjoy! :)

Here is a Haskell implementation of this:

    
~~~{.Haskell}
data Validation e a = Fail e | Success a deriving (Eq, Show)

-- Associative, binary operation (Monoid without identity)
class Semigroup a where
  append :: a -> a -> a

(<+>) :: Validation e a -> (a -> b) -> Validation e b
Fail e <+> _ = Fail e
Success a <+> f = Success (f a)

(<*>) :: (Semigroup e) => Validation e a
    -> Validation e (a -> b)
    -> Validation e b
Fail e1 <*> Fail e2 = Fail (e1 `append` e2)
Fail e1 <*> Success _ = Fail e1
Success _ <*> Fail e2 = Fail e2
Success a <*> Success f = Success (f a)
~~~



And Scala:


    
~~~{.Scala}
sealed trait Validation[E, A] {
  def <+>[B](f: A => B): Validation[E, B] = this match {
    case Fail(e) => Fail(e)
    case Success(a) => Success(f(a))
  }

  def <*>[B](f: Validation[E, A => B])(implicit s: Semigroup[E])
    : Validation[E, B] = this match {
    case Fail(e1) => f match {
      case Fail(e2) => Fail(s append (e1, e2))
      case Success(_) => Fail(e1)
    }
    case Success(a) => f match {
      case Fail(e2) => Fail(e2)
      case Success(f) => Success(f(a))
    }
  }
}
case class Fail[E, A](e: E) extends Validation[E, A]
case class Success[E, A](a: A) extends Validation[E, A]

// Associative binary operation (Monoid without identity)
case class Semigroup[A](append: (A, A) => A)
~~~



And now, for the allegedly practical Java. Hold your breath:


    
~~~{.Java}
// Java pre-amble, sigh.
interface F<X, Y> {
  Y apply(X x);
}

// Associative binary operation (Monoid without identity)
interface Semigroup<a> {
  A append(A a1, A a2);
}

abstract class Validation<E, A> {
  // No algebraic data types in Java. Use a Church encoding (catamorphism).
  public abstract <x> X fold(F<E, X> fail, F<A, X> success);

  // Construction for fail
  public static <E, A> Validation<E, A> fail(final E e) {
    return new Validation<E, A>() {
      public <x> X fold(final F<E, X> fail, final F<A, X> success) {
        return fail.apply(e);
      }
    };
  }

  // Construction for success
  public static <E, A> Validation<E, A> success(final A a) {
    return new Validation<E, A>() {
      public <x> X fold(final F<E, X> fail, final F<A, X> success) {
        return success.apply(a);
      }
    };
  }

  // <+>
  public <b> Validation<E, B> map(final F<A, B> f) {
    return new Validation<E, B>() {
      public <x> X fold(final F<E, X> fail, final F<B, X> success) {
        return Validation.this.fold(fail, new F<A, X>() {
          public X apply(final A a) {
            return success.apply(f.apply(a));
          }
        });
      }
    };
  }

  // <*>
  public <b> Validation<E, B> applicativate(
      final Validation<E, F<A, B>> f,
      final Semigroup<e> s /* no implicits or type-classes in Java */) {
    return Validation.this.fold(new F<E, Validation<E, B>>() {
      public Validation<E, B> apply(final E e1) {
        return f.fold(new F<E, Validation<E, B>>() {
          public Validation<E, B> apply(final E e2) {
            // case (Fail(e1), Fail(e2))
            return Validation.fail(s.append(e1, e2));
          }
        }, new F<F<A, B>, Validation<E, B>>() {
          public Validation<E, B> apply(final F<A, B> f) {
            // case (Fail(e1), Success(f))
            return Validation.fail(e1);
          }
        });
      }
    }, new F<A, Validation<E, B>>() {
      public Validation<E, B> apply(final A a) {
        return f.fold(new F<E, Validation<E, B>>() {
          public Validation<E, B> apply(final E e2) {
            // case (Success(a), Fail(e2))
            return Validation.fail(e2);
          }
        }, new F<F<A, B>, Validation<E, B>>() {
          public Validation<E, B> apply(final F<A, B> f) {
            // case (Success(a), Success(f))
            return Validation.success(f.apply(a));
          }
        });
      }
    });
  }
}
~~~

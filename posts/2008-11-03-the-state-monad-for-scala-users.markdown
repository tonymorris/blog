---
comments: true
date: 2008-11-03 12:55:14
layout: post
slug: the-state-monad-for-scala-users
title: The State Monad for Scala users
wordpressid: 396
tags: Programming
---

[Scalaz 3.2](http://wiki.workingmouse.com/index.php/Scalaz) includes support for a State data type for the [Scala Programming Language](http://scala-lang.org/). This data type is a monad and thus supports `flatMap` and can be used in a for-comprehension.

Below I will give a practical demonstration of why you might choose to use the State data type as a monad.

Consider a binary leaf tree data type:

    
~~~{.Scala}
sealed abstract class Tree[+A]
final case class Leaf[A](a: A) extends Tree[A]
final case class Branch[A](left: Tree[A], right: Tree[A]) extends Tree[A]
~~~



This is pretty simple so far. Now suppose we wanted to map a function across a `Tree` from left to right **where the result depended on the previous result**. For this example, consider that we wanted to number each leaf by adding 1 as we traverse left to right (at a given starting value). That is, the result depends on the previous result because we must add 1 to that previous result.

We might implement this by passing in the integer value and returning the pair of the `Tree` as it is constructed and the integer value as it increments. Such an implementation might look like this:


    
~~~{.Scala}
sealed abstract class Tree[+A] {
  def number(seed: Int): (Tree[(A, Int)], Int) = this match {
    case Leaf(x) => (Leaf(x, seed), seed + 1)
    case Branch(left, right) => left number seed match {
      case (l, ls) => {
        right number ls match {
          case (r, rs) => (Branch(l, r), rs)
        }
      }
    }
  }
}
~~~



This code is pretty messy and it would become even messier with a less trivial example to apply across the `Tree`.

The `State` data type is effectively a `Function[S, (S, A)]` and the monad instance runs across the `Function[_, (_, A)]` part. That is to say, the `flatMap` signature looks roughly like this:

    
~~~{.Scala}
trait State[S, A] {
  val s: Function[S, (S, A)] // abstract
  def flatMap[B](f: A => Function[S, (S, B)]): Function[S, (S, B)]
}
~~~
    


You might consider filling out this method signature for fun :)

The `flatMap` function allows the user to make the state change implicit rather than explicit (and messy!). The `State` data type includes a few useful methods and functions. I will only use two of those functions below; `init`, which constructs a `State` object that has computed the state itself. For example, going with the analogy to `Function[S, (S, A)]`, the `init` function looks like this: `s => (s, s)` and so returns a `State[S, S]`. The second function is `modify`, which applies a transform the state and it is intended to ignore the computed value (`Unit`).

Here is our new implementation:

    
~~~{.Scala}
import scalaz.State
import scalaz.State._

def numbers: State[Int, Tree[(A, Int)]] = this match {
  case Leaf(x) => for(s <- init[Int];
                      n <- modify((_: Int) + 1))
                  yield Leaf((x, s + 1))
  case Branch(left, right) => for(l <- left.numbers;
                                  r <- right.numbers)
                              yield Branch(l, r)
}
~~~



This is much neater and hides the otherwise explicit recursive application of adding 1 to an integer. Following is a complete source file that can be compiled successfully against Scalaz 3.2 using the latest version of Scala.


    
~~~{.Scala}
sealed abstract class Tree[+A] {
  def number(seed: Int): (Tree[(A, Int)], Int) = this match {
    case Leaf(x) => (Leaf(x, seed), seed + 1)
    case Branch(left, right) => left number seed match {
      case (l, ls) => {
        right number ls match {
          case (r, rs) => (Branch(l, r), rs)
        }
      }
    }
  }

  import scalaz.State
  import scalaz.State._

  def numbers: State[Int, Tree[(A, Int)]] = this match {
    case Leaf(x) => for(s <- init[Int];
                        n <- modify((_: Int) + 1))
                    yield Leaf((x, s + 1))
    case Branch(left, right) => for(l <- left.numbers;
                                    r <- right.numbers)
                                yield Branch(l, r)
  }
}
final case class Leaf[A](a: A) extends Tree[A]
final case class Branch[A](left: Tree[A], right: Tree[A]) extends Tree[A]
~~~



And if you find Haskell easier to read, this might help too. It is a roughly equivalent program using GHC's built-in `State` data type.


    
~~~{.Haskell}
import Control.Monad.State.Lazy

data Tree a = Leaf a | Branch (Tree a) (Tree a)

number :: Int -> Tree a -> (Tree (a, Int),Int)
number seed (Leaf a) = (Leaf (a, seed), seed + 1)
number seed (Branch left right)
 = let (l, ls) = number seed left
       (r, rs) = number ls right
   in
       (Branch l r, rs)

numbers :: Tree a -> State Int (Tree (a, Int))
numbers (Leaf a) = do n <- get
                      modify (+1)
                      return (Leaf (a, n))

numbers (Branch l r) = do left <- numbers l
                          right <- numbers r
                          return (Branch left right)

initState :: State s s
initState = State (\s -> (s, s))
~~~

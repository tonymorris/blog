---
comments: true
date: 2013-02-22 19:00.00
layout: post
slug: memoisation-with-state-using-scala
title: Memoisation with State using Scala
tags: Programming, Scala, State, Memoisation, Fibonacci
---

Everyone has seen a naïve fibonacci implementation

~~~{.Scala}
object FibNaïve {
  def fibnaïve(n: BigInt): BigInt =
    if(n <= 1)
      n
    else {
      val r = fibnaïve(n - 1)
      val s = fibnaïve(n - 2)
      r + s
    }
}
~~~

While this implementation is elegant, it is exponential in time with respect to `n`. For example, computing the result of `fibnaïve(4)` will result in the unnecessary re-computation of values less than `4`. If we unravel the recursion, computation occurs as follows:

~~~
  fibnaïve(4)
= fibnaïve(3) + fibnaïve(2)
= (fibnaïve(2) + fibnaïve(1)) + (fibnaïve(1) + fibnaïve(0))
= ((fibnaïve(1) + fibnaïve(0)) + fibnaïve(1)) + (fibnaïve(1) + fibnaïve(0))
~~~

This algorithm calculates for `fibnaïve(2)` twice, which ultimately results in a lot of repeated calculations, especially as `n` grows. What we would like to do is trade some space to store previous stored values for a given `n`. We can achieve this by looking up the argument value in a table and if it has already been computed, we return it then carry on, but if it hasn't, we compute the result by calling `fibnaïve`, store it in the table, then return it. This technique is called *memoisation*.

As a first cut, let's solve fibonacci with a helper function that passes a `Map[BigInt, BigInt]` around in the recursion. This map will serve at the memoisation table.

~~~{.Scala}
object FibMemo {
  type Memo = Map[BigInt, BigInt]

  def fibmemo1(n: BigInt): BigInt = {
    def fibmemoR(z: BigInt, memo: Memo): (BigInt, Memo) =
      if(z <= 1)
        (z, memo)
      else memo get z match {
        case None => {
          val (r, memo0) = fibmemoR(z - 1, memo)
          val (s, memo1) = fibmemoR(z - 2, memo0)
          (r + s, memo1)
        }
        case Some(v) => (v, memo)
      }

    fibmemoR(n, Map())._1
  }
}
~~~

We have traded space (the memoisation table) for speed; the algorithm is more efficient by not recomputing values. However, we have sacrificed the elegance of the code. How can we achieve both elegance and efficiency?

## The State Monad

The previous code (`fibmemo1`) has *passed state through the computation*. In other words, where we once returned a value of the type `A`, we are accepting an argument of the type `Memo` and returning the pair `(A, Memo)`. The state in this case is a value of the type `Memo`. We can represent this as a data structure:

~~~{.Scala}
case class State[S, A](run: S => (A, S))
~~~

Our `fibmemoR` function which once had this type:

~~~{.Scala}
def fibmemoR(z: BigInt, memo: Memo): (BigInt, Memo)
~~~

…can be transformed to this type:

~~~{.Scala}
def fibmemoR(z: BigInt): State[Memo, BigInt]
~~~

Let's write our new fibonacci function:

~~~{.Scala}
object FibMemo {
  type Memo = Map[BigInt, BigInt]

  def fibmemo2(n: BigInt): BigInt = {
    def fibmemoR(z: BigInt): State[Memo, BigInt] =
      State(memo =>
        if(z <= 1)
          (z, memo)
        else memo get z match {
          case None => {
            val (r, memo0) = fibmemoR(z - 1) run memo
            val (s, memo1) = fibmemoR(z - 2) run memo
            (r + s, memo1)
          }
          case Some(v) => (v, memo)
        })

    fibmemoR(n).run(Map())._1
  }
}
~~~

Ew! This code is still rather clumsy as it manually passes the memo table around. What can we do about it? This is where the state monad is going to help us out. The state monad is going to take care of passing the state value around for us. The monad itself is implemented by three functions:

1. The `map` method on `State[S, A]`.

2. The `flatMap` method on `State[S, A]`.

3. The `insert` function on the `object State` that *inserts a value while leaving the state unchanged*.

I will also add two convenience methods:

1. `eval` method for running the `State` value and dropping the resulting state value.

2. `memo` function for memoising the computed value if not already computed.

Here goes:

~~~{.Scala}
case class State[S, A](run: S => (A, S)) {
  // 1. the map method
  def map[B](f: A => B): State[S, B] =
    State(s => {
      val (a, t) = run(s)
      (f(a), t)
    })

  // 2. the flatMap method
  def flatMap[B](f: A => State[S, B]): State[S, B] =
    State(s => {
      val (a, t) = run(s)
      f(a) run t
    })

  // Convenience function to drop the resulting state value
  def eval(s: S): A =
    run(s)._1
}

object State {
  // 3. The insert function
  def insert[S, A](a: A): State[S, A] =
    State(s => (a, s))

  // Memoising function computes and memoises if not already
  def memo[A, B](a: A, f: Map[A, B] => B): State[Map[A, B], B] =
    State(m => m get a match {
      case None => {
        val r = f(m)
        (r, m + ((a, r)))
      }
      case Some(b) =>
        (b, m)
    })
}
~~~

We can see that the `flatMap` method takes care of passing the state value through a given function. This is the ultimate purpose of the state monad. Specifically, the state monad allows the programmer to pass a state (`S`) value through a computation (`A`) without us having to manually handle it. The `map` and `insert` methods complete the state monad.

How does our fibonacci implementation look now?

~~~{.Scala}
object FibMemo {
  type Memo = Map[BigInt, BigInt]

  def fibmemo3(: BigInt): BigInt = {
    def fibmemoR(z: BigInt): State[Memo, BigInt] =
      if(z <= 1)
        State.insert(z)
      else State.memo(z, m => {
          val k =
            fibmemoR(z - 1) flatMap (r =>
            fibmemoR(z - 2) map     (s =>
            r + s))
          k eval m
        })
    fibmemoR(n) eval Map()
  }
}
~~~

Now we have used the three state monad methods to pass the memo table through the computation for us - no more manual handling of passing that memo table through to successive recursive calls.

Scala provides syntax for the type of computation that chains calls to `flatMap` and `map`. It is implemented using the `for` and `yield` keywords in what is called a *for-comprehension*. The for-comprehension syntax will make the calls to `flatMap` and `map`, while allowing a more imperative-looking style. For example, where we once wrote code such as `x flatMap (r =>`, we will now write `r <- x` inside the for-comprehension.

How does this look?

~~~{.Scala}
object FibMemo {
  type Memo = Map[BigInt, BigInt]

  def fibmemo4(n: BigInt): BigInt = {
    def fibmemoR(z: BigInt): State[Memo, BigInt] =
      if(z <= 1)
        State.insert(z)
      else State.memo(z, m => {
          val k =
            for {
              r <- fibmemoR(z - 1)
              s <- fibmemoR(z - 2)
            } yield r + s
          k eval m
        })
    fibmemoR(n) eval Map()
  }
}
~~~

This is a lot neater as the memoisation table is handled by the state monad. In fact, it is starting to look like the original naïve solution. We are no longer manually handling the state transitions, which allows us to express the essence of the problem and without the calculation speed blow-out.

Where you once may have use `var`, consider if the state monad is instead more appropriate.

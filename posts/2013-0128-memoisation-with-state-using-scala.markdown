---
comments: true
date: 2013-01-28 19:00.00
layout: post
slug: memoisation-with-state-using-scala
title: Memoisation with State using Scala
tags: Programming, Scala
---

Everyone has seen a naïve fibonacci implementation

~~~{.Scala}
object FibNaïve {
  def fibnaïve(n: BigInt): BigInt =
    if(n <= 1)
      n
    else
      fibnaïve(n - 1) + fibnaïve(n - 2)
}
~~~

While this implementation is elegant, it is exponential in time with respect to `n`. For example, computing the result of `fibnaïve(4)` will result in the unnecessary re-computation of values less than `4`. If we unravel the recursion, computation occurs as follows:

~~~
  fibnaïve(4)
= fibnaïve(3) + fibnaïve(2)
= (fibnaïve(2) + fibnaïve(1)) + (fibnaïve(1) + fibnaïve(0))
= ((fibnaïve(1) + fibnaïve(0)) + fibnaïve(1)) + (fibnaïve(1) + fibnaïve(0))
~~~

What we would like to do is trade some space to store previous stored values for a given `n`. That is to say, when we compute `fibnaïve(n)`, we look up in a table to determine if the result for `n` has been computed earlier. We can achieve this by looking it up in a table and if it has already been computed, we return it then carry on, but if it hasn't, we compute the result, store it in the table, then return it. This technique is called *memoisation*.

Memoisation is a technique that arises often in *Dynamic Programming Algorithms*. That is, algorithms for which the solution is


~~~{.Scala}
// Fibonacci function
object Fibq {
  def fibq(n: BigInt): BigInt =
    if(n <= 1)
      n
    else
      fibq(n - 1) + fibq(n - 2)
}
case class State[S, A](run: S => (A, S))  {
    def map[B](f: A => B): State[S, B] =
      State(s => {
        val (a, t) = run(s)
        (f(a), t)
      })

    def ap[B](f: State[S, B]): State[S, B] =
      State(s => {
        val (g, t) = f run s
        val (a, u) = this run t
        (g(a), u)
      })

    def flatMap[B](f: A => State[S, B]): State[S, B] =
      State(s => {
        val (a, t) = run(s)
        f(a) run t
    })

    def eval(s: S): A =
      run(s)._1

    def something[B](x: State[S, A => B]): State[S, B] = State { s =>
      val (f, s0) = x run s
      val (a, s1) = this run s0
      (f(a), s1)
    }
}
object State {
  def point[S, A](a: A): State[S, A] =
    State(s => (a, s))
}
object Fib {
  def insertWithDefault[K, V](k: K, v: => V, m: Map[K, V]): (V, Map[K, V]) =
    m get k match {
      case None => (v, m + ((k, v)))
      case Some(vv) => (vv, m)
    }
  def insertWithDefaultq[K, V](k: K, v: => V, m: Map[K, V]): (V, Map[K, V]) =
    m get k map( (_,m) ) getOrElse( (v, m + ((k, v))) )

  type Memo = Map[BigInt, BigInt]

  def fib(n: BigInt): BigInt = {
    def fibmemo(z: BigInt): State[Memo, BigInt] =
      if(List(0, 1) exists (BigInt(_) == z))
        State.point(z)
      else
        State(memo => memo get z match {
          case None => {
            val k =
              for {
                r <- fibmemo(z - 1)
                s <- fibmemo(z - 2)
              } yield r + s
            val l = k eval memo
            (l, memo + ((z, l)))
          }
          case Some(v) => (v, memo)
        })

    fibmemo(n) eval Map.empty
  }

  def fibi(n: BigInt): BigInt = {
    def fibmemo(z: BigInt): State[Memo, BigInt] =
      if(List(0, 1) exists (BigInt(_) == z))
        State.point(z)
      else
        State(memo => insertWithDefault(z, (for {
          r <- fibmemo(z - 1)
          s <- fibmemo(z - 2)
        } yield r + s) eval memo, memo))

    fibmemo(n) eval Map.empty
  }
}
~~~

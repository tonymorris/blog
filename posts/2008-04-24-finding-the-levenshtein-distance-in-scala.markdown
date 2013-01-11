---
comments: true
date: 2008-04-24 09:01:59
layout: post
slug: finding-the-levenshtein-distance-in-scala
title: Finding the Levenshtein Distance in Scala
wordpressid: 77
tags: Programming
---

In the spirit of esoteric code snippets like [this one](http://www.poromenos.org/node/87), I thought I'd put my two bob in :)

I have written the Levenshtein Distance algorithm using [Scala](http://scala-lang.org/) below. The Levensthein Distance algorithm is a [Dynamic Programming Algorithm](http://en.wikipedia.org/wiki/Dynamic_programming) (DPA). This implementation is a little different to the Python one (which creates the arrays explicitly and fills them using loops):



	
  * The code more closely represents the mathematical definition of the algorithm

	
  * The code is easier to reason about because the destructive updates occur behind the scenes (in the memoisation library)

	
  * The code below requires a third party library (Scalaz) _note that Scalaz comes with demo code including the levenshtein distance and other DPAs_

	
  * The code has a better complexity than the typical loopy version by using lazy evaluation (notice that 'c' is not always evaluated)

	
  * While the code memoises with an array, you could use say, a map and save some space as well

	
  * The code builds the call stack as it traverses the matrix (the loopy one does not)




    
~~~{.Scala}
import scalaz.memo.Memo
import scalaz.memo.SizedMemo.arraySizedMemo

object Levenshtein {
  def levenshtein[A](x: Array[A], y: Array[A]): Int = {
    val im = arraySizedMemo
    val m = im[Memo[Int, Int]](x.length + 1)
    // the call matrix
    def mx(i: Int, j: Int): Int = if(i == 0) j else if(j == 0) i else {
      def f = (n: Int) => im[Int](y.length + 1)
      val a = m(f)(i - 1)(mx(i - 1, _))(j) + 1
      val b = m(f)(i - 1)(mx(i - 1, _))(j - 1) + (if(x(i - 1) == y(j - 1)) 0 else 1)
      lazy val c = m(f)(i)(mx(i, _))(j - 1) + 1
      if(a < b) a else if(b <= c) b else c
    }
    mx(x.length, y.length)
  }

  def main(args: Array[String]) =
    println(levenshtein(args(0).toCharArray, args(1).toCharArray))
}
~~~



To run this code:

    
    
    $ wget http://projects.workingmouse.com/public/scalaz/artifacts/2.4/scalaz.jar # download Scalaz 2.4
    $ scalac -classpath scalaz.jar Levenshtein.scala # compile
    $ scala -classpath .:scalaz.jar Levenshtein algorithm altruistic # find the distance!
    <strong>6</strong>
    




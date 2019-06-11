---
comments: true
date: 2009-02-14 17:16:26
layout: post
slug: funky-scala-bifunctor
title: Funky Scala Bifunctor
wordpressid: 542
tags: Programming
---

A co-variant binary functor (aka bifunctor) is any type constructor with a kind * -> * -> * that can satisfy identity and composition when mapping on either type variable. A couple of examples of bifunctors in the Scala library are




  * `Tuple2`


  * `Either`



Note that `Function1` is not a bifunctor, evident by the fact that it is contra-variant in its first type variable. It is notable that `Tuple2` represents _conjunction_ while `Either` represents the _disjunction_ of two theorems under the [Curry-Howard Isomorphism](http://en.wikipedia.org/wiki/Curry-Howard_correspondence) (`Function1` represents _implication_`).

A bifunctor often combines the two possible mappings into a function called `bimap` with the signature (Scala-ish syntax):


    
~~~{.Scala}
F[A, B] => (A => C) => (B => D) => F[C, D]
~~~



Recall that identity and composition laws must satisfy in the implementation.

Consider now an implementation of `bimap` for both `Tuple2` and  `Either`:


    
~~~{.Scala}
trait Bifunctor[F[+_, +_]] {
  def bimap[A, B, C, D](fa: F[A, B], f: A => C, g: B => D): F[C, D]
}

object Bifunctor {
  implicit def Tuple2Bifunctor = new Bifunctor[Tuple2] {
    def bimap[A, B, C, D](fa: (A, B), f: A => C, g: B => D) =
      (f(fa._1), g(fa._2))
  }

  implicit def EitherBifunctor = new Bifunctor[Either] {
    def bimap[A, B, C, D](fa: Either[A, B], f: A => C, g: B => D) =
      fa match {
        case Left(a) => Left(f(a))
        case Right(b) => Right(g(b))
      }
  }
}
~~~



These implementations are quite easy to understand, but they are not so pretty to use. This is an unfortunate consequence of a number of factors about Scala, including:




  * Higher-kinds are never inferred


  * Functions cannot be used in infix position



In Scala, methods that end with a colon (:) can have their arguments swapped when not using dot notation. For example, the following two lines are equivalent:

    
    
    obj.::(arg) 
    arg :: obj
    



This is evident when using `scala.List` and the cons (`::`) method.

We might wrap up our `Bifunctor` implementation similarly and create some nice syntax for mapping on the bifunctor. For example, let us define three methods on any `Bifunctor` value:





  * `<-:` for mapping on one side


  * `:->` for mapping on the other side


  * `<-:->` for mapping on both sides




    
~~~{.Scala}
trait BifunctorW[F[+_, +_], A, B] {
  val value: F[A, B]
  val bifunctor: Bifunctor[F]

  def <-:->[C, D](f: A => C, g: B => D) = bifunctor.bimap(value, f, g)

  def <-:[C](f: A => C) = bifunctor.bimap(value, f, identity[B])

  def :->[D](g: B => D) = bifunctor.bimap(value, identity[A], g)
}

object BifunctorW {
  // a somewhat verbose but handy trick
  // for partially applying type variables.
  def bifunctor[F[+_, +_]] = new BifunctorApply[F] {
    def apply[A, B](v: F[A, B])(implicit b: Bifunctor[F]) =
        new BifunctorW[F, A, B] {
      val value = v
      val bifunctor = b
    }
  }

  trait BifunctorApply[F[+_, +_]] {
    def apply[A, B](v: F[A, B])(implicit b: Bifunctor[F]): BifunctorW[F, A, B]
  }

  implicit def Tuple2Bifunctor[A, B](v: (A, B)) =
    bifunctor[Tuple2](v)

  implicit def Either2Bifunctor[A, B](v: Either[A, B]) =
    bifunctor[Either](v)
}
~~~



So we've wrapped a value and its `Bifunctor` implementation. So how does it look when we use it? Like this!


    
~~~{.Scala}
object Main {
  def main(args: Array[String]) {
    import BifunctorW._

    val x: Either[Int, String] = Left(7)
    val y = ("hello", 42)

    val f = (n: Int) => n + 1
    val g = (s: String) => s.reverse
    val h = (s: String) => s.toUpperCase
    val i = (n: Int) => n * 2

   // Pretty neat eh?
    val p = f <-: x :-> g
    val q = h <-: y :-> i

    // $ scala Main
    // Left(8)
    // (HELLO,84)
    println(p)
    println(q)
  }
}
~~~



And I think that's an elegant use of a sometimes baroque (when trying for advanced concepts) language :)

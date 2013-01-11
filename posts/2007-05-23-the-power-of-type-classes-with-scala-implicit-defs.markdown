---
comments: true
date: 2007-05-23 11:02:15
layout: post
slug: the-power-of-type-classes-with-scala-implicit-defs
title: The Power of Type-classes with Scala implicit defs
wordpressid: 30
tags: Programming
---

A core part of Haskell's type system is that of type-classes. If you have never used type-classes, then there is [a pretty good description in Yet Another Haskell Tutorial (YAHT)](http://en.wikibooks.org/wiki/Haskell/YAHT/Type_basics#Type_Classes). Haskell type-classes have been likened to Java/C# interfaces or Scala traits, but there are some significant differences.

With Scala's implicit definitions (defs), it is possible to come pretty close to emulating Haskell's type-classes. This ability can make writing certain constructs very simple and also, extremely flexible. Imagine you were asked to write a function that found the maximum value in a List of values. In Scala, we might write it like this:

    
~~~{.Scala}
object O {
  def maximum(x: List[Int]): Int = x match {
    case Nil => error("maximum undefined for empty list")
    case x :: y :: ys => maximum((if(x > y) x else y) :: ys)
    case x :: _ => x
  }
}
~~~


Notice that if we wish to write the function for some other type besides `Int`, we would have to start again. Or, we could _generalise_ to the Scala `Ordered` type:

    
~~~{.Scala}
object O {
  def maximum[a <: Ordered[a]](x: List[a]): a = x match {
    case Nil => error("maximum undefined for empty list")
    case x :: y :: ys => maximum((if(x > y) x else y) :: ys)
    case x :: _ => x
  }
}
~~~


So we accept now that we can find the maximum of any type, so long as that type implements the `Ordered` trait/interface. But what if we have a type that doesn't implement that interface? Worse still, what if our type does implement that interface, but we wish to redefine ordering? The only solution here is to create a new type that wraps the old type and redefines ordering. In Haskell, this is called "newtyping", because it is done with the `newtype` keyword and is one line of very trivial code (`newtype T = T U`) plus the two or three for the redefinition of ordering. However, in other languages, this is a little cumbersome and can lead to the bloating of code that is difficult to understand.

Scala implicit defs come to the rescue here. But first, a little introduction to two core Haskell type-classes called `Eq` and `Ord`. Here is an excerpt of their definitions:

    
~~~{.Haskell}
class  Eq a  where
  (==), (/=)   :: a -> a -> Bool
  -- omitted as superfluous to the topic

class  (Eq a) => Ord a  where
  compare      :: a -> a -> Ordering
  -- omitted as superfluous to the topic
~~~


This first declaration is of the `Eq` type-class and states that to implement the class over an unbound type (a), one must define equality such that gives two instances of (a), return a boolean value indicating equality (or non-equality). The second declaration is of the `Ord` type-class and states that to implement the class over a type (a) that is bound by the type-class `Eq`, one must define ordering such that given two instances of (a), return a type `Ordering` (which has only three possible values; LT, EQ, GT).

We can express something similar in Scala. However, note that the type parameter (a) is not co-variant, while all Haskell type parameters are co-variant. This is because Haskell is a _pure functional programming language_, while Scala is not.

    
~~~{.Scala}
trait Eq[a] {
  def eq(a1: a)(a2: a): Boolean
}

trait Ord[a] extends Eq[a] {
  override def eq(a1: a)(a2: a) = compare(a1)(a2) == EQ
  def compare(a1: a)(a2: a): Ordering
}

// for completeness
sealed abstract class Ordering
final case object LT extends Ordering
final case object EQ extends Ordering
final case object GT extends Ordering
~~~


Now comes the interesting part (sorry to the impatient :)). Imagine now that we could write our maximum function, where we could say "the maximum of a list of any type (a) such that there exists **one and only one implementation** of the `Ord` trait for type (a) **implicitly defined in compiler scope**". That is to say, if there exists two or more implementations of `Ord`, fail the compiler with an ambiguity error (indeed, many Scala newcomers have likely observed these ambiguity errors from the core Scala APIs). If there is no implementation within scope, fail as well.

Here is how we would write the function now:

    
~~~{.Scala}
object Maximum {
  def maximum[a](as: List[a])(implicit o: Ord[a]): a = as match {
    case Nil => error("maximum undefined for empty list")
    case x :: y :: ys => maximum((o.compare(x)(y) match {
      case GT => x
      case _ => y
    }) :: ys)
    case x :: _ => x
  }
}
~~~


Notice that the function takes two arguments, however, one of them is declared **implicit**. This means that we do not need to explicitly provide this argument (though we can and we might to resolve an ambiguity, but there are also other means). So long as there exists one implementation of `Ord[a]` implicitly defined in compiler scope, then this implementation is used.

Let us write such an implementation:

    
~~~{.Scala}
object Ord {
  implicit def intOrd = new Ord[Int] {
    def compare(a1: Int)(a2: Int) = if(a1 == a2) EQ else if(a1 < a2) LT else GT
  }
}
~~~


Notice that the definition of `intOrd` is declared with the **implicit** keyword. Now, if the `intOrd` function is in scope (and no other implementation is), then we can call the `maximum` function with a list of integers. Indeed, if we had some other definition of ordering for integers (which is rarely the case for integers, but imagine it were some other type), we would not have to rewrite anything to continue using our `maximum` function.

Also, be aware that implicit definitions are **not** transitive. This means that although we might have an implicit definition from `Int` to `Ord[Int]` and we might have some other implicit definition from T to `Int`, however, this does not imply that we have an implicit definition from T to `Ord[Int]`. This attribute is very unfortunate and quite limiting of the potential of implicit definitions, but one that is to be accepted nonetheless.

At this point, some would be thinking, "so what?". After all, some have no problem with newtyping (or adapting or design patterning or whatever you want to call it). However, I draw your attention to one example of many where this approach would save much more work. Let us take a look at that [`List.range`](http://www.scala-lang.org/docu/files/api/scala/List$object.html#range%28Int%2CInt%29) function. Notice how the `range` function is defined over integers only? If I wanted to get a range for some other type, I'd have to rewrite the `range` function, or I'd have to write a conversion to type `Int` and use that. Both very annoying.

Indeed, the `range` function can be generalised to any type that has the notion of a successor (as well, a predecessor if we wish to support backward ranges). However, if we were to define a range function with a step n (default: n = 1), then invoking our successor or predecessor function n times may lead to an inefficiency that is resolved by the integer type, because it has the constant-time + function. Indeed, we would need another function successor(n) so that this efficiency gain could be utilised for types that support it, such as the integer type, where it would use the + function. By default, this implementation would simply invoke the successor function n times. We will acknowledged this fact but will ignore it hereon.

Let us define our successor function:

    
~~~{.Scala}
trait Successor[a] {
  def succ(a: a): Option[a]

  // for efficiency reasons, we might define this function
  // A default implementation invokes the succ function n times.
  // The integer implementation would override and use +
  // def succn(n: Int)(a: a): Option[a]
}
~~~


The `succ` function returns a succeeding value of type (a) if it exists, but no value if there is no successor i.e. the type is maximally bounded. This is the case for the integer type where `Integer.MAX_VALUE` has no successor. We would write the implementation like so:

    
~~~{.Scala}
object Successor {
  implicit def intSuccessor = new Successor[Int] {
    override def succ(a: Int) = a match {
      case Integer.MAX_VALUE => None
      case _ => Some(a + 1)
    }
  }
}
~~~


Now, we can write our `range` function such that it is defined over **any** type, so long as that type has one and only one implementation of both `Successor[a]` and `Ord[a]` in scope. The implementation of `Ord[a]` is required to determine the ordering of the `from` parameter to the `to` parameter. Also, the equality relation (defined by `Eq[a]`) is required to know when the range stops.

    
~~~{.Scala}
object Range {
  def range[a](from: a)(to: a)(implicit e: Successor[a], o: Ord[a]): List[a] =
    if(o.compare(from)(to) == GT) Nil
    else if(o.eq(from)(to)) List(from)
    else e.succ(from) match {
      case None => List(from)
      case Some(s) => from :: range(s)(to)
    }
}
~~~


Suppose we wish to find the range of a list of playing cards. Is it Aces high or Aces low? What about other card orderings used in the myriad of different card games? What about other types besides playing cards? It doesn't matter, since **our `range` function is as general as it can be**. Yay! No more writing the same code over and over!

    
~~~{.Scala}
sealed abstract class Rank
final case object ACE extends Rank
final case object TWO extends Rank
final case object THREE extends Rank
final case object FOUR extends Rank
final case object FIVE extends Rank
final case object SIX extends Rank
final case object SEVEN extends Rank
final case object EIGHT extends Rank
final case object NINE extends Rank
final case object TEN extends Rank
final case object JACK extends Rank
final case object QUEEN extends Rank
final case object KING extends Rank

object Rank {
  implicit def acesHigh = new Successor[Rank] with Ord[Rank] {
    val ranks = Array(TWO, THREE, FOUR, FIVE, SIX, SEVEN, EIGHT, NINE, JACK, QUEEN, KING, ACE)

    override def compare(a1: Rank)(a2: Rank) = if(a1 == a2) EQ else if(ranks.indexOf(a1) < ranks.indexOf(a2)) LT else GT

    override def succ(a: Rank) = a match {
      case ACE => None
      case _ => Some(ranks(ranks.indexOf(a) + 1))
    }
  }

  implicit def acesLow = new Successor[Rank] {
    val ranks = Array(ACE, TWO, THREE, FOUR, FIVE, SIX, SEVEN, EIGHT, NINE, JACK, QUEEN, KING)

    override def succ(a: Rank) = a match {
      case KING => None
      case _ => Some(ranks(ranks.indexOf(a) + 1))
    }
  }
}
~~~


Notice here that we can bring either `acesHigh` or `acesLow` into compiler scope (but not both) and use the `range` function successfully. Indeed, the following code prints different output because of which function is in scope:

    
~~~{.Scala}
object Main {
  def main(args: Array[String]) = {
    {
      import Rank.acesHigh
      Console.println(range(TWO: Rank)(THREE)) // List(TWO,THREE)
      Console.println(range(TWO: Rank)(ACE)) // List(TWO,THREE,FOUR,FIVE,SIX,SEVEN,EIGHT,NINE,JACK,QUEEN,KING,ACE)
      Console.println(range(ACE: Rank)(KING)) // List()
    }

    {
      import Rank.acesLow
      Console.println(range(TWO: Rank)(THREE)) // List(TWO,THREE)
      Console.println(range(TWO: Rank)(ACE)) // List(TWO,THREE,FOUR,FIVE,SIX,SEVEN,EIGHT,NINE,JACK,QUEEN,KING)
      Console.println(range(ACE: Rank)(KING)) // List()
    }
  }
}
~~~


Reader's exercise: write a `range` function with step of type `Int`, which may be negative (and so step backward)

Here is a complete, compilable source code listing:

    
    
    trait Eq[a] {
      def eq(a1: a)(a2: a): Boolean
    }
    
    sealed abstract class Ordering
    final case object LT extends Ordering
    final case object EQ extends Ordering
    final case object GT extends Ordering
    
    trait Ord[a] extends Eq[a] {
      override def eq(a1: a)(a2: a) = compare(a1)(a2) == EQ
      def compare(a1: a)(a2: a): Ordering
    }
    
    object Ord {
      implicit def intOrd = new Ord[Int] {
        def compare(a1: Int)(a2: Int) = if(a1 == a2) EQ else if(a1 < a2) LT else GT
      }
    }
    
    object Maximum {
      def maximum[a](as: List[a])(implicit o: Ord[a]): a = as match {
        case Nil => error("maximum undefined for empty list")
        case x :: y :: ys => maximum((o.compare(x)(y) match {
          case GT => x
          case _ => y
        }) :: ys)
        case x :: _ => x
      }
    }
    
    trait Successor[a] {
      def succ(a: a): Option[a]
    }
    
    object Successor {
      implicit def intSuccessor = new Successor[Int] {
        override def succ(a: Int) = a match {
          case Integer.MAX_VALUE => None
          case _ => Some(a + 1)
        }
      }
    }
    
    object Range {
      def range[a](from: a)(to: a)(implicit e: Successor[a], o: Ord[a]): List[a] =
        if(o.compare(from)(to) == GT) Nil
        else if(o.eq(from)(to)) List(from)
        else e.succ(from) match {
          case None => List(from)
          case Some(s) => from :: range(s)(to)
        }
    }
    
    sealed abstract class Rank
    final case object ACE extends Rank
    final case object TWO extends Rank
    final case object THREE extends Rank
    final case object FOUR extends Rank
    final case object FIVE extends Rank
    final case object SIX extends Rank
    final case object SEVEN extends Rank
    final case object EIGHT extends Rank
    final case object NINE extends Rank
    final case object TEN extends Rank
    final case object JACK extends Rank
    final case object QUEEN extends Rank
    final case object KING extends Rank
    
    object Rank {
      implicit def acesHigh = new Successor[Rank] with Ord[Rank] {
        val ranks = Array(TWO, THREE, FOUR, FIVE, SIX, SEVEN, EIGHT, NINE, JACK, QUEEN, KING, ACE)
    
        override def compare(a1: Rank)(a2: Rank) = if(a1 == a2) EQ else if(ranks.indexOf(a1) < ranks.indexOf(a2)) LT else GT
    
        override def succ(a: Rank) = a match {
          case ACE => None
          case _ => Some(ranks(ranks.indexOf(a) + 1))
        }
      }
    
      implicit def acesLow = new Successor[Rank] {
        val ranks = Array(ACE, TWO, THREE, FOUR, FIVE, SIX, SEVEN, EIGHT, NINE, JACK, QUEEN, KING)
    
        override def succ(a: Rank) = a match {
          case KING => None
          case _ => Some(ranks(ranks.indexOf(a) + 1))
        }
      }
    }
    
    object Main {
      def main(args: Array[String]) = {
        import Ord.intOrd
        Console.println(Maximum.maximum(List.range(1, 10)))
    
        import Range.range
    
        Console.println(range(7)(8))
        Console.println(range(7)(7))
        Console.println(range(7)(6))
        Console.println(range(Integer.MAX_VALUE)(Integer.MAX_VALUE))
        Console.println(range(Integer.MAX_VALUE - 1)(Integer.MAX_VALUE))
        Console.println(range(Integer.MAX_VALUE)(7))
        Console.println(range(Integer.MIN_VALUE)(Integer.MIN_VALUE + 1))
    
        {
          import Rank.acesHigh
          Console.println(range(TWO: Rank)(THREE))
          Console.println(range(TWO: Rank)(ACE))
          Console.println(range(ACE: Rank)(KING))
        }
    
        {
          import Rank.acesLow
          Console.println(range(TWO: Rank)(THREE))
          Console.println(range(TWO: Rank)(ACE))
          Console.println(range(ACE: Rank)(KING))
        }
      }
    }
    

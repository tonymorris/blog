---
comments: true
date: 2010-04-03 15:45:00
layout: post
slug: monad-exercises-in-scala-addendum
title: Monad Exercises in Scala (addendum)
wordpressid: 739
tags: Programming
---

Following is a test harness that should print a series of `PASS` when executed against a correct solution to the original [Monad Exercises in Scala](http://blog.tmorris.net/monad-exercises-in-scala/). These exercises include a Haskell version and a test harness for this is also found below.

I have also written a [complete solution in Scala](http://paste.pocoo.org/show/197013/) and [same again for Haskell](http://paste.pocoo.org/show/197014/) _**(clicking either link will give away the answer)**_.


    
~~~{.Scala}
object Main {
  def main(args: Array[String]) {
    import Monad._
    import MonadicFunctions._

    val plusOne = Inter(1+)
    val multTwo = Inter(2*)
    val squared = Inter(n => n*n)
    val plus = (_: Int) + (_: Int)

    val values = List(
// sequence
sequence(List(List(1, 2), List(3, 4)), ListMonad),
sequence(List(Some(7), Some(8), Some(9)), OptionMonad),
sequence(List(Some(7), None, Some(9)), OptionMonad),
sequence(List(plusOne, multTwo, squared), InterMonad) f 7,
sequence(List(Identity(7), Identity(4)), IdentityMonad),
// fmap
fmap(List(1, 2, 3), (x: Int) => x * 10, ListMonad),
fmap(Some(8), (x: Int) => x * 10, OptionMonad),
fmap(None: Option[Int], (x: Int) => x * 10, OptionMonad),
fmap(plusOne, (x: Int) => x * 10, InterMonad) f 7,
fmap(Identity(9), (x: Int) => x * 10, IdentityMonad),
// flatten
flatten(List(List(1, 2), List(3, 4)), ListMonad),
flatten(Some(Some(8)), OptionMonad),
flatten(Some(None: Option[Int]), OptionMonad),
flatten(None: Option[Option[Int]], OptionMonad),
flatten(Inter(a => Inter(a *)), InterMonad) f 7,
flatten(Identity(Identity(8)), IdentityMonad),
// apply
apply(List((a: Int) => a + 1,
           (a: Int) => a * 2,
           (a: Int) => a % 2), List(1, 2, 3), ListMonad),
apply(Some((a: Int) => a + 1), Some(8), OptionMonad),
apply(None: Option[Int => Int], Some(8), OptionMonad),
apply(Some((a: Int) => a + 1), None: Option[Int], OptionMonad),
apply(Inter(a => (b: Int) => a * b), Inter(1+), InterMonad) f 7,
apply(Identity((a: Int) => a + 1), Identity(7), IdentityMonad),
// filterM
filterM((a: Int) => List(a > 2, a % 2 == 0), List(1, 2, 3), ListMonad),
filterM((a: Int) => Some(a > 1), List(1, 2, 3), OptionMonad),
filterM((a: Int) => Inter(n => a * n % 2 == 0),
  List(1, 2, 3), InterMonad) f 7,
filterM((a: Int) => Identity(a > 1), List(1, 2, 3), IdentityMonad),
// replicateM
replicateM(2, List(7, 8), ListMonad),
replicateM(2, Some(7), OptionMonad),
replicateM(2, plusOne, InterMonad) f 7,
replicateM(2, Identity(6), IdentityMonad),
// lift2
lift2(plus, List(1, 2), List(3, 4), ListMonad),
lift2(plus, Some(7), Some(8), OptionMonad),
lift2(plus, Some(7), None: Option[Int], OptionMonad),
lift2(plus, None: Option[Int], Some(8), OptionMonad)
    )

    val verify = List(
// sequence
List(List(1, 3), List(1, 4), List(2, 3), List(2, 4)),
Some(List(7, 8, 9)),
None,
List(8, 14, 49),
Identity(List(7, 4)),
// fmap
List(10, 20, 30),
Some(80),
None,
80,
Identity(90),
// flatten
List(1, 2, 3, 4),
Some(8),
None,
None,
49,
Identity(8),
// apply
List(2, 3, 4, 2, 4, 6, 1, 0, 1),
Some(9),
None,
None,
56,
Identity(8),
// filterM
List(List(3), Nil, List(2, 3), List(2), List(3),
  Nil, List(2, 3), List(2)),
Some(List(2, 3)),
List(2),
Identity(List(2, 3)),
// replicateM
List(List(7, 7), List(7, 8), List(8, 7), List(8, 8)),
Some(List(7, 7)),
List(8, 8),
Identity(List(6, 6)),
// lift2
List(4, 5, 5, 6),
Some(15),
None,
None
)

    for((a, b) <- values zip verify)
      println(if(a == b) "PASS"
              else "FAIL. Expected: " + b + " Actual: " + a)
  }
}
~~~



Haskell `main` function which should print a series of `PASS` when executed against the original [Monad Exercises in Scala](http://blog.tmorris.net/monad-exercises-in-scala/).


    
~~~{.Haskell}
main :: IO ()
main =
  let plusOne = Inter (1+)
      multTwo = Inter (2*)
      squared = Inter (\n -> n*n)
      s x = show x
      (%) = f
      values =
        [
        -- sequence'
        s (sequence' [[1, 2], [3, 4]] listMonad),
        s (sequence' [Just 7, Just 8, Just 9] maybeMonad),
        s (sequence' [Just 7, Nothing, Nothing] maybeMonad),
        s (sequence' [plusOne, multTwo, squared] interMonad % 7),
        s (sequence' [Identity 7, Identity 4] identityMonad),
        -- fmap'
        s (fmap' [1..3] (*10) listMonad),
        s (fmap' (Just 8) (*10) maybeMonad),
        s (fmap' Nothing (*10) maybeMonad),
        s (fmap' plusOne (*10) interMonad % 7),
        s (fmap' (Identity 9) (*10) identityMonad),
        -- flatten
        s (flatten [[1, 2], [3, 4]] listMonad),
        s (flatten (Just (Just 8)) maybeMonad),
        s (flatten (Just (Nothing :: Maybe Int)) maybeMonad),
        s (flatten (Nothing :: Maybe (Maybe Int)) maybeMonad),
        s (flatten (Inter (Inter . (*))) interMonad % 7),
        s (flatten (Identity (Identity 8)) identityMonad),
        -- apply
        s (apply [(+1), (*2), (`mod` 2)] [1..3] listMonad),
        s (apply (Just (+1)) (Just 8) maybeMonad),
        s (apply (Nothing :: Maybe (Int -> Int)) (Just 8) maybeMonad),
        s (apply (Just (+1)) (Nothing :: Maybe Int) maybeMonad),
        s (apply (Inter (*)) (Inter (1+)) interMonad % 7),
        s (apply (Identity (+1)) (Identity 7) identityMonad),
        -- filterM'
        s (filterM' (\a -> [a > 2, a `mod` 2 == 0]) [1..3] listMonad),
        s (filterM' (\a -> Just (a > 1)) [1..3] maybeMonad),
        s (filterM' (\a -> Inter (\n -> a * n `mod` 2 == 0)) [1..3]
          interMonad % 7),
        s (filterM' (Identity . (>1)) [1..3] identityMonad),
        -- replicateM'
        s (replicateM' 2 [7, 8] listMonad),
        s (replicateM' 2 (Just 7) maybeMonad),
        s (replicateM' 2 plusOne interMonad % 7),
        s (replicateM' 2 (Identity 6) identityMonad),
        -- lift2
        s (lift2 (+) [1, 2] [3, 4] listMonad),
        s (lift2 (+) (Just 7) (Just 8) maybeMonad),
        s (lift2 (+) (Just 7) (Nothing :: Maybe Int) maybeMonad),
        s (lift2 (+) (Nothing :: Maybe Int) (Just 8) maybeMonad)
        ]
      verify =
        [
        -- sequence'
        s ([[1, 3], [1, 4], [2, 3], [2, 4]]),
        s (Just [7..9]),
        s (Nothing :: Maybe Int),
        s [8, 14, 49],
        s (Identity [7, 4]),
        -- fmap'
        s [10, 20, 30],
        s (Just 80),
        s (Nothing :: Maybe Int),
        s 80,
        s (Identity 90),
        -- flatten
        s [1..4],
        s (Just 8),
        s (Nothing :: Maybe Int),
        s (Nothing :: Maybe Int),
        s 49,
        s (Identity 8),
        -- apply
        s [2, 3, 4, 2, 4, 6, 1, 0, 1],
        s (Just 9),
        s (Nothing :: Maybe Int),
        s (Nothing :: Maybe Int),
        s 56,
        s (Identity 8),
        -- filterM'
        s [[3], [], [2, 3], [2], [3], [], [2, 3], [2]],
        s (Just [2, 3]),
        s [2],
        s (Identity [2, 3]),
        -- replicateM
        s [[7, 7], [7, 8], [8, 7], [8, 8]],
        s (Just [7, 7]),
        s [8, 8],
        s (Identity [6, 6]),
        -- lift2
        s [4, 5, 5, 6],
        s (Just 15),
        s (Nothing :: Maybe Int),
        s (Nothing :: Maybe Int)
        ]
  in mapM_
      (\(a, b) ->
        print(if a == b
                then "PASS"
                else "FAIL. Expected: " ++ b ++ " Actual: " ++ a))
      (values `zip` verify)
~~~

---
comments: true
date: 2010-03-25 11:42:16
layout: post
slug: monad-exercises-in-scala
title: Monad exercises in Scala
wordpressid: 716
tags: Programming
---

A result of discussion in the #scala IRC channel.

_A colleague has asked me to convert this to Haskell, which I have done below._





  * The `trait Monad` answers the question "What does monad mean?"


  * The `object Monad` answers the question "What are some examples?"


  * The `object MonadicFunctions` answers the question "What are the practical implications?"



If there are any questions or you get stuck, the `#scala` or `#scalaz` channel on Freenode could help out.


    
~~~{.Scala}
// 1. Start here. Observe this trait
trait Monad[M[_]] {
  def flatMap[A, B](a: M[A], f: A => M[B]): M[B]
  def unital[A](a: A): M[A]
}

// A simple data type, which turns out to satisfy the above trait
case class Inter[A](f: Int => A)

// So does this.
case class Identity[A](a: A)

// Monad implementations
object Monad {
  // 2. Replace error("todo") with an implementation
  def ListMonad: Monad[List] = error("todo")

  // 3. Replace error("todo") with an implementation
  def OptionMonad: Monad[Option] = error("todo")

  // 4. Replace error("todo") with an implementation
  def InterMonad: Monad[Inter] = error("todo")

  // 5. Replace error("todo") with an implementation
  def IdentityMonad: Monad[Identity] = error("todo")
}

object MonadicFunctions {
  // 6. Replace error("todo") with an implementation
  def sequence[M[_], A](as: List[M[A]], m: Monad[M]): M[List[A]] =
    error("todo")

  // 7. Replace error("todo") with an implementation
  def fmap[M[_], A, B](a: M[A], f: A => B, m: Monad[M]): M[B] =
    error("todo")

  // 8. Replace error("todo") with an implementation
  def flatten[M[_], A](a: M[M[A]], m: Monad[M]): M[A] =
    error("todo")

  // 9. Replace error("todo") with an implementation
  def apply[M[_], A, B](f: M[A => B], a: M[A], m: Monad[M]): M[B] =
    error("todo")

  // 10. Replace error("todo") with an implementation
  def filterM[M[_], A](f: A => M[Boolean], as: List[A]
    , m: Monad[M]): M[List[A]] =
    error("todo")

  // 11. Replace error("todo") with an implementation
  def replicateM[M[_], A](n: Int, a: M[A], m: Monad[M]): M[List[A]] =
    error("todo: flatMap n times to produce a list")

  // 12. Replace error("todo") with an implementation
  def lift2[M[_], A, B, C](f: (A, B) => C, a: M[A], b: M[B]
    , m: Monad[M]): M[C] =
    error("todo")

  // lift3, lift4, etc. Interesting question: Can we have liftN?
}
~~~



**Haskell**





  * The `data Monad'` answers the question "What does monad mean?"


  * The functions under `*** Monad implementations ***` answers the question "What are some examples?"


  * The functions under `*** Monadic Functions ***` answers the question "What are the practical implications?"




    
~~~{.Haskell}
{-# LANGUAGE RankNTypes #-}

-- 1. Start here. Observe this data type
data Monad' m = Monad' {
  unital :: forall a. a -> m a,
  flatMap :: forall a b. m a -> (a -> m b) -> m b
}

-- A simple data type, which turns out to satisfy the above trait
newtype Inter a = Inter { f :: Int -> a }

-- So does this.
newtype Identity a = Identity { a :: a }
  deriving Show

-- *** Monad implementations ***

-- 2. Replace error "todo" with an implementation
listMonad :: Monad' []
listMonad = error "todo"

-- 3. Replace error "todo" with an implementation
maybeMonad :: Monad' Maybe
maybeMonad = error "todo"

-- 4. Replace error "todo" with an implementation
interMonad :: Monad' Inter
interMonad = error "todo"

-- 5. Replace error "todo" with an implementation
identityMonad :: Monad' Identity
identityMonad = error "todo"

-- *** Monadic functions ***

-- 6. Replace error "todo" with an implementation
sequence' :: [m a] -> Monad' m -> m [a]
sequence' = error "todo"

-- 7. Replace error "todo" with an implementation
fmap' :: m a -> (a -> b) -> Monad' m -> m b
fmap' = error "todo"

-- 8. Replace error "todo" with an implementation
flatten :: m (m a) -> Monad' m -> m a
flatten = error "todo"

-- 9. Replace error "todo" with an implementation
apply :: m (a -> b) -> m a -> Monad' m -> m b
apply = error "todo"

-- 10. Replace error "todo" with an implementation
filterM' :: (a -> m Bool) -> [a] -> Monad' m -> m [a]
filterM' = error "todo"

-- 11. Replace error "todo" with an implementation
replicateM' :: Int -> m a -> Monad' m -> m [a]
replicateM' = error "todo: flatMap n times to produce a list"

-- 12. Replace error "todo" with an implementation
lift2 :: (a -> b -> c) -> m a -> m b -> Monad' m -> m c
lift2 = error "todo"

-- lift3, lift4, etc. Interesting question: Can we have liftN?
~~~

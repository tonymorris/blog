---
comments: true
date: 2013-06-05 20:00:00
layout: post
slug: refactoring-puzzle
title: Refactoring Puzzle
tags: Programming, Scala, Haskell, C#, Java
---

The following programming puzzle is about code duplication. The goal is to identify and remove the code duplication.
The exercise is given in a number of programming languages and the means by which the problem is solved may depend on
the specific programming language. It might even be the case that the best solution is "do nothing", since the
programming language does not provide the support for alleviating the code duplication. Therefore, a complete solution
to this puzzle is a solution for each of the programming languages.

The two functions `runOptions` and `runIntRdrs` implement a specific function with a small difference between each. The
duplication in their code bodies is noted in the comments and is denoted by asterisks.

**How might the problem of code duplication be solved for this case?**

The puzzle is designed to compile as-is, which means for some languages, support data structures need to be provided.
For example, the C# programming language does not provide an immutable (cons) list data structure, so the bare minimum
is supplied here. This makes the puzzle appear quite noisy for that specific programming language, however be assured it
is the same code.

# Scala

~~~{.Scala}
object RefactorPuzzle {
  case class IntRdr[+A](read: Int => A) {
    def map[B](f: A => B): IntRdr[B] =
      IntRdr(f compose read)

    def flatMap[B](f: A => IntRdr[B]): IntRdr[B] =
      IntRdr(n => f(read(n)).read(n))
  }

  object IntRdr {
    def apply[A](a: A): IntRdr[A] =
      IntRdr(_ => a)
  }

  // Return all the Some values, or None if not all are Some.
  def runOptions[A](x: List[Option[A]]): Option[List[A]] =
    x.foldRight[Option[List[A]]](Option(Nil))((a, b) => a.flatMap(aa => b.map(aa :: _)))

  // Apply an Int to a list of int readers and return the list of return values.
  def runIntRdrs[A](x: List[IntRdr[A]]): IntRdr[List[A]] =
    x.foldRight[IntRdr[List[A]]](IntRdr(Nil))((a, b) => a.flatMap(aa => b.map(aa :: _)))

// Code Duplication

// *******      *************      *******      ***********
// def runOptions[A](x: List[Option[A]]): Option[List[A]] =
// def runIntRdrs[A](x: List[IntRdr[A]]): IntRdr[List[A]] =

// ************      ***********      *************************************************
// x.foldRight[Option[List[A]]](Option(Nil))((a, b) => a.flatMap(aa => b.map(aa :: _)))
// x.foldRight[IntRdr[List[A]]](IntRdr(Nil))((a, b) => a.flatMap(aa => b.map(aa :: _)))

}
~~~

# Haskell

~~~{.Haskell}
module RefactoringPuzzle where

newtype IntRdr a =
  IntRdr {
    readIntRdr :: Int -> a
  }

mapIntRdr ::
  IntRdr a
  -> (a -> b)
  -> IntRdr b
mapIntRdr (IntRdr g) f =
  IntRdr (f . g)

bindIntRdr ::
  IntRdr a
  -> (a -> IntRdr b)
  -> IntRdr b
bindIntRdr (IntRdr g) f =
  IntRdr (\n -> readIntRdr (f (g n)) n)

applyIntRdr ::
  a
  -> IntRdr a
applyIntRdr =
  IntRdr . const

type Option = Maybe

mapOption ::
  Option a
  -> (a -> b)
  -> Option b
mapOption Nothing _ =
  Nothing
mapOption (Just a) f =
  Just (f a)

bindOption ::
  Option a
  -> (a -> Option b)
  -> Option b
bindOption Nothing _ =
  Nothing
bindOption (Just a) f =
  f a

applyOption ::
  a
  -> Option a
applyOption a =
  Just a

-- Return all the Some values, or None if not all are Some.
runOptions :: [Option a] -> Option [a]
runOptions = foldr (\a b -> bindOption a (\aa -> mapOption b (aa:))) (applyOption [])

-- Apply an Int to a list of int readers and return the list of return values.
runIntRdrs :: [IntRdr a] -> IntRdr [a]
runIntRdrs = foldr (\a b -> bindIntRdr a (\aa -> mapIntRdr b (aa:))) (applyIntRdr [])

-- Code Duplication

-- ***      ******      *******      ****
-- runOptions :: [Option a] -> Option [a]
-- runIntRdrs :: [IntRdr a] -> IntRdr [a]

-- ***      ***********************      **************      ************           ****
-- runOptions = foldr (\a b -> bindOption a (\aa -> mapOption b (aa:))) (applyOption [])
-- runIntRdrs = foldr (\a b -> bindIntRdr a (\aa -> mapIntRdr b (aa:))) (applyIntRdr [])
~~~

# C\#

~~~{.cs}
using System;

namespace RefactoringPuzzle {
class IntRdr<A> {
  public readonly Func<int, A> read;

  IntRdr(Func<int, A> read) {
    this.read = read;
  }

  public IntRdr<B> Select<B>(Func<A, B> f) {
    return new IntRdr<B>(n => f(read(n)));
  }

  public IntRdr<B> SelectMany<B>(Func<A, IntRdr<B>> f) {
    return new IntRdr<B>(n => f(read(n)).read(n));
  }

  public static IntRdr<A> apply(A a) {
    return new IntRdr<A>(_ => a);
  }
}

abstract class Option<A> {
  public abstract X Fold<X>(Func<A, X> some, X none);

  public Option<B> Select<B>(Func<A, B> f) {
    return Fold<Option<B>>(a => new Option<B>.Some(f(a)), new Option<B>.None());
  }

  public Option<B> SelectMany<B>(Func<A, Option<B>> f) {
    return Fold(f, new Option<B>.None());
  }

  public static Option<A> apply(A a) {
    return new Some(a);
  }

  public class Some : Option<A> {
    readonly A a;

    public Some(A a) {
      this.a = a;
    }

    public override X Fold<X>(Func<A, X> some, X none) {
      return some(a);
    }
  }

  public class None : Option<A> {
    public override X Fold<X>(Func<A, X> some, X none) {
      return none;
    }
  }
}

abstract class List<A> {
  public abstract X FoldRight<X>(Func<A, X, X> f, X x);

  // Return all the Some values, or None if not all are Some.
  Option<List<A>> runOptions(List<Option<A>> x) {
    return x.FoldRight((a, b) => a.SelectMany(aa =>
      b.Select(bb => bb.Prepend(aa))), Option<List<A>>.apply(new Nil()));
  }

  // Apply an Int to a list of int readers and return the list of return values.
  IntRdr<List<A>> runIntRdrs(List<IntRdr<A>> x) {
    return x.FoldRight((a, b) => a.SelectMany(aa =>
      b.Select(bb => bb.Prepend(aa))), IntRdr<List<A>>.apply(new Nil()));
  }

  public List<A> Prepend(A a) {
    return new Cons(a, this);
  }

  public class Nil : List<A> {
    public override X FoldRight<X>(Func<A, X, X> f, X x) {
      return x;
    }
  }

  public class Cons : List<A> {
    readonly A head;
    readonly List<A> tail;

    public Cons(A head, List<A> tail) {
      this.head = head;
      this.tail = tail;
    }

    public override X FoldRight<X>(Func<A, X, X> f, X x) {
      return f(head, tail.FoldRight(f, x));
    }
  }
}

// Code Duplication

//       *************      *******      *********
// Option<List<A>> runOptions(List<Option<A>> x) {
// IntRdr<List<A>> runIntRdrs(List<IntRdr<A>> x) {

// ***********************************************
// return x.FoldRight((a, b) => a.SelectMany(aa =>
// return x.FoldRight((a, b) => a.SelectMany(aa =>

// *********************************      ****************************
// b.Select(bb => bb.Prepend(aa))), Option<List<A>>.apply(new Nil()));
// b.Select(bb => bb.Prepend(aa))), IntRdr<List<A>>.apply(new Nil()));

}
~~~

# Java

~~~{.Java}
package RefactoringPuzzle;

abstract class Func<T, U> {
  abstract U apply(T t);
}

abstract class IntRdr<A> {
  abstract A read(int i);

  <B> IntRdr<B> map(final Func<A, B> f) {
    return new IntRdr<B>() {
      B read(int i) {
        return f.apply(IntRdr.this.read(i));
      }
    };
  }

  <B> IntRdr<B> bind(final Func<A, IntRdr<B>> f) {
    return new IntRdr<B>() {
      B read(int i) {
        return f.apply(IntRdr.this.read(i)).read(i);
      }
    };
  }

  static <A> IntRdr<A> apply(final A a) {
    return new IntRdr<A>() {
      A read(int _) {
        return a;
      }
    };
  }
}

abstract class Option<A> {
  abstract <X> X fold(Func<A, X> some, X none);

  <B> Option<B> map(final Func<A, B> f) {
    return new Option<B>() {
      <X> X fold(final Func<B, X> some, X none) {
        return Option.this.fold(new Func<A, X>(){
          X apply(A a) {
            return some.apply(f.apply(a));
          }
        }, none);
      }
    };
  }

  <B> Option<B> bind(final Func<A, Option<B>> f) {
    return new Option<B>() {
      <X> X fold(final Func<B, X> some, final X none) {
        return Option.this.fold(new Func<A, X>(){
          X apply(A a) {
            return f.apply(a).fold(some, none);
          }
        }, none);
      }
    };
  }

  static <A> Option<A> apply(final A a) {
    return new Option<A>() {
      <X> X fold(Func<A, X> some, X none) {
        return some.apply(a);
      }
    };
  }
}

abstract class List<A> {
  abstract <X> X foldRight(Func<A, Func<X, X>> f, X x);

  // Return all the Some values, or None if not all are Some.
  static <A> Option<List<A>> runOptions(List<Option<A>> x) {
    return x.foldRight(new Func<Option<A>, Func<Option<List<A>>, Option<List<A>>>>(){
      Func<Option<List<A>>, Option<List<A>>> apply(final Option<A> a) {
        return new Func<Option<List<A>>, Option<List<A>>>() {
          Option<List<A>> apply(final Option<List<A>> b) {
            return a.bind(new Func<A, Option<List<A>>>(){
              Option<List<A>> apply(final A aa) {
                return b.map(new Func<List<A>, List<A>>(){
                  List<A> apply(List<A> bb) {
                    return bb.prepend(aa);
                  }
                });
              }
            });
          }
        };
      }
    }, Option.apply(List.<A>nil()));
  }

  // Apply an Int to a list of int readers and return the list of return values.
  static <A> IntRdr<List<A>> runIntRdrs(List<IntRdr<A>> x) {
    return x.foldRight(new Func<IntRdr<A>, Func<IntRdr<List<A>>, IntRdr<List<A>>>>(){
      Func<IntRdr<List<A>>, IntRdr<List<A>>> apply(final IntRdr<A> a) {
        return new Func<IntRdr<List<A>>, IntRdr<List<A>>>() {
          IntRdr<List<A>> apply(final IntRdr<List<A>> b) {
            return a.bind(new Func<A, IntRdr<List<A>>>(){
              IntRdr<List<A>> apply(final A aa) {
                return b.map(new Func<List<A>, List<A>>(){
                  List<A> apply(List<A> bb) {
                    return bb.prepend(aa);
                  }
                });
              }
            });
          }
        };
      }
    }, IntRdr.apply(List.<A>nil()));
  }

  List<A> prepend(final A a) {
    return new List<A>() {
      <X> X foldRight(Func<A, Func<X, X>> f, X x) {
        return f.apply(a).apply(this.foldRight(f, x));
      }
    };
  }

  static <A> List<A> nil() {
    return new List<A>() {
      <X> X foldRight(Func<A, Func<X, X>> f, X x) {
        return x;
      }
    };
  }
}

// Code Duplication

// ***********      *************      *******      *********
// static <A> Option<List<A>> runOptions(List<Option<A>> x) {
// static <A> IntRdr<List<A>> runIntRdrs(List<IntRdr<A>> x) {

//   ****************************      **********      ***********      **************
//   return x.foldRight(new Func<Option<A>, Func<Option<List<A>>, Option<List<A>>>>(){
//   return x.foldRight(new Func<IntRdr<A>, Func<IntRdr<List<A>>, IntRdr<List<A>>>>(){

//     *****      ***********      ***********************      ********
//     Func<Option<List<A>>, Option<List<A>>> apply(final Option<A> a) {
//     Func<IntRdr<List<A>>, IntRdr<List<A>>> apply(final IntRdr<A> a) {

//       ****************      ***********      **************
//       return new Func<Option<List<A>>, Option<List<A>>>() {
//       return new Func<IntRdr<List<A>>, IntRdr<List<A>>>() {

//               **********************      **************
//         Option<List<A>> apply(final Option<List<A>> b) {
//         IntRdr<List<A>> apply(final IntRdr<List<A>> b) {

//           **************************      *************
//           return a.bind(new Func<A, Option<List<A>>>(){
//           return a.bind(new Func<A, IntRdr<List<A>>>(){

//                   *****************************
//             Option<List<A>> apply(final A aa) {
//             IntRdr<List<A>> apply(final A aa) {

//               ******************************************
//               return b.map(new Func<List<A>, List<A>>(){
//               return b.map(new Func<List<A>, List<A>>(){

//                 ***************************
//                 List<A> apply(List<A> bb) {
//                 List<A> apply(List<A> bb) {

//                   **********************
//                   return bb.prepend(aa);
//                   return bb.prepend(aa);
  …
//   ***      ***********************
//   }, Option.apply(List.<A>nil()));
//   }, IntRdr.apply(List.<A>nil()));
  …
~~~

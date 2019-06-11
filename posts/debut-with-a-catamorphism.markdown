---
comments: true
date: 2009-12-02 20:36:27
layout: post
slug: debut-with-a-catamorphism
title: Debut with a Catamorphism
wordpressid: 594
tags: Health, Programming
---

Hello everyone,

Many of you know I have been battling health problems for the last 2 years subsequent to a sporting injury. After a total of six surgical procedures, my most recent about 3 weeks ago, I am glad to be almost completely cured. Today I lodged a formal complaint to the Medical Health Board of Queensland against six doctors (coincidentally, the same number of operations -- there were a total of 19 doctors mentioned).

As a result of the aforementioned, I haven't written in a while, so I plan to return to my usual from now on.

I thought I'd write a little about a pervasive problem on the Scala mailing lists. Specifically, the misunderstanding of the purpose of static typing. Of course, there will always be lots of myths and proponents willing to ensure their survival, however, I am a strong believer that education is the only means by which we can advance the software development industry, despite the task often appearing insurmountable (ala Scala mailing list).

Some discussions recently on the Scala mailing list include the relationship between `scala.Option` data type and `null` and another discussion about documentation and types that are once-inhabited such as `forall a b c. (a -> b) -> (b -> c) -> a -> c`

I thought I'd propose a small piece of code and leave any potential insights unstated so as not to destroy anything for the observer. There is a Haskell version toward the end.

I propose the following data type:

~~~{.Scala}
trait MyOption[+A] {
  // single abstract method
  def cata[X](some: A => X, none: => X): X
}
~~~

Astute observers will notice that the `cata` method is similar to a combination of the `map` and `getOrElse` methods on `scala.Option`. This topic has also arisen on the Scala mailing list in the past. In other words, I could have written this:

~~~{.Scala}
trait MyOption[+A] {
  // two abstract methods
  def map[X](f: A => X): MyOption[X]
  def getOrElse[AA >: A](a: => AA): AA
}
~~~

...and I'd have a data structure that is exactly the same as the previous one. What may not be obvious is that `MyOption` is also exactly the same as `scala.Option`. The correct term for "exactly the same" is _isomorphic_. Yes, this is despite not using case classes or subclasses -- coincidentally, another area where much mythology is prevalent.

I could also write construction functions that are akin to `scala.None` and `scala.Some`:

~~~{.Scala}
object MyOption {
  def none[A] = new MyOption[A] {
    def cata[X](s: A => X, n: => X) = n
  }

  def some[A](a: A) = new MyOption[A] {
    def cata[X](s: A => X, n: => X) = s(a)
  }
}
~~~

As an exercise, I propose filling out the `Option` API, however, using the single abstract method `cata` and the `none`/`some` constructor functions for additional convenience. I promise you it can be done and that anything `scala.Option` can do, `MyOption` can do also (and vice versa), since they are isomorphic.

~~~{.Scala}
trait MyOption[+A] {
  // single abstract method
  def cata[X](some: A => X, none: => X): X

  def map[B](f: A => B): MyOption[B] = error("todo")

  def flatMap[B](f: A => MyOption[B]): MyOption[B] = error("todo")

  def getOrElse[AA >: A](e: => AA): AA = error("todo")

  def filter(p: A => Boolean): MyOption[A] = error("todo")

  def foreach(f: A => Unit): Unit = error("todo")

  def isDefined: Boolean = error("todo")

  def isEmpty: Boolean = error("todo")

  // WARNING: not defined for None
  def get: A = error("todo")

  def orElse[AA >: A](o: MyOption[AA]): MyOption[AA] = error("todo")

  def toLeft[X](right: => X): Either[A, X] = error("todo")

  def toRight[X](left: => X): Either[X, A] = error("todo")

  def toList: List[A] = error("todo")

  def iterator: Iterator[A] = error("todo")
}

object MyOption {
  def none[A] = new MyOption[A] {
    def cata[X](s: A => X, n: => X) = n
  }

  def some[A](a: A) = new MyOption[A] {
    def cata[X](s: A => X, n: => X) = s(a)
  }
}
~~~

If you get stuck, the answer is base-64 encoded below, however, I encourage you to follow the types and to the extent that there may be ambiguity, follow the existing Scala API specification for `scala.Option`. If need be, please ask questions. Best of luck!

~~~
dHJhaXQgTXlPcHRpb25bK0FdIHsKICAvLyBzaW5nbGUgYWJzdHJhY3QgbWV0aG9kCiAgZGVmIGNh
dGFbWF0oc29tZTogQSA9PiBYLCBub25lOiA9PiBYKTogWAoKICBpbXBvcnQgTXlPcHRpb24uXwoK
ICBkZWYgbWFwW0JdKGY6IEEgPT4gQik6IE15T3B0aW9uW0JdID0gY2F0YShmIGFuZFRoZW4gc29t
ZSwgbm9uZSkKCiAgLy8gQWxzbwogIC8vIGRlZiBtYXBbQl0oZjogQSA9PiBCKTogTXlPcHRpb25b
Ql0gPSBmbGF0TWFwKGYgYW5kVGhlbiBzb21lKQoKICBkZWYgZmxhdE1hcFtCXShmOiBBID0+IE15
T3B0aW9uW0JdKTogTXlPcHRpb25bQl0gPSBjYXRhKGYsIG5vbmUpCgogIGRlZiBnZXRPckVsc2Vb
QUEgPjogQV0oZTogPT4gQUEpOiBBQSA9IGNhdGEocyA9PiBzLCBlKQoKICBkZWYgZmlsdGVyKHA6
IEEgPT4gQm9vbGVhbik6IE15T3B0aW9uW0FdID0gY2F0YShhID0+IGlmKHAoYSkpIHNvbWUoYSkg
ZWxzZSBub25lLCBub25lKQoKICBkZWYgZm9yZWFjaChmOiBBID0+IFVuaXQpOiBVbml0ID0gY2F0
YShmLCAoKSkKCiAgZGVmIGlzRGVmaW5lZDogQm9vbGVhbiA9IGNhdGEoXyA9PiB0cnVlLCBmYWxz
ZSkKCiAgZGVmIGlzRW1wdHk6IEJvb2xlYW4gPSBjYXRhKF8gPT4gZmFsc2UsIHRydWUpCgogIC8v
IFdBUk5JTkc6IG5vdCBkZWZpbmVkIGZvciBOb25lCiAgZGVmIGdldDogQSA9IGNhdGEoYSA9PiBh
LCBlcnJvcigiTm9uZS5nZXQiKSkKCiAgZGVmIG9yRWxzZVtBQSA+OiBBXShvOiBNeU9wdGlvbltB
QV0pOiBNeU9wdGlvbltBQV0gPSBjYXRhKF8gPT4gdGhpcywgbykKCiAgZGVmIHRvTGVmdFtYXShy
aWdodDogPT4gWCk6IEVpdGhlcltBLCBYXSA9IGNhdGEoTGVmdChfKSwgUmlnaHQocmlnaHQpKQoK
ICBkZWYgdG9SaWdodFtYXShsZWZ0OiA9PiBYKTogRWl0aGVyW1gsIEFdID0gY2F0YShSaWdodChf
KSwgTGVmdChsZWZ0KSkKCiAgZGVmIHRvTGlzdDogTGlzdFtBXSA9IGNhdGEoTGlzdChfKSwgTmls
KQoKICBkZWYgaXRlcmF0b3I6IEl0ZXJhdG9yW0FdID0gY2F0YShJdGVyYXRvci5zaW5nbGUoXyks
IEl0ZXJhdG9yLmVtcHR5KQp9CgpvYmplY3QgTXlPcHRpb24gewogIGRlZiBub25lW0FdID0gbmV3
IE15T3B0aW9uW0FdIHsKICAgIGRlZiBjYXRhW1hdKHM6IEEgPT4gWCwgbjogPT4gWCkgPSBuCiAg
fQoKICBkZWYgc29tZVtBXShhOiBBKSA9IG5ldyBNeU9wdGlvbltBXSB7CiAgICBkZWYgY2F0YVtY
XShzOiBBID0+IFgsIG46ID0+IFgpID0gcyhhKQogIH0KfQoK
~~~



For Haskell users:


    
~~~{.Haskell}
{-# LANGUAGE RankNTypes #-}

-- Data.Maybe
newtype Option a = Option { cata :: forall x. (a -> x) -> x -> x }

-- Just
some :: a -> Option a
some a = Option (\s _ -> s a)

-- Nothing
none :: Option a
none = Option (const id)

-- fmap
map' :: (a -> b) -> Option a -> Option b
map' f m = error "todo"

-- (>>=)
flatMap :: (a -> Option b) -> Option a -> Option b
flatMap f m = error "todo"

-- fromMaybe
getOrElse :: Option a -> a -> a
getOrElse = error "todo"

filter :: Option a -> (a -> Bool) -> Option a
filter m p = error "todo"

-- mapM_
foreach :: Option a -> (a -> IO ()) -> IO ()
foreach m f = error "todo"

-- isJust
isDefined :: Option a -> Bool
isDefined m = error "todo"

-- isNothing
isEmpty :: Option a -> Bool
isEmpty m = error "todo"

-- WARNING: not defined for None
-- fromJust
get :: Option a -> a
get m = error "todo"

-- mplus
orElse :: Option a -> Option a -> Option a
orElse m n = error "todo"

toLeft :: Option a -> x -> Either a x
toLeft m x = error "todo"

toRight :: Option a -> x -> Either x a
toRight m x = error "todo"

-- maybeToList
toList :: Option a -> [a]
toList m = error "todo"

iterator = error "bzzt. This is Haskell silly."
~~~




    
~~~
ey0jIExBTkdVQUdFIFJhbmtOVHlwZXMgIy19CgotLSBEYXRhLk1heWJlCm5ld3R5cGUgT3B0aW9u
IGEgPSBPcHRpb24geyBjYXRhIDo6IGZvcmFsbCB4LiAoYSAtPiB4KSAtPiB4IC0+IHggfQoKLS0g
SnVzdApzb21lIDo6IGEgLT4gT3B0aW9uIGEKc29tZSBhID0gT3B0aW9uIChccyBfIC0+IHMgYSkK
Ci0tIE5vdGhpbmcKbm9uZSA6OiBPcHRpb24gYQpub25lID0gT3B0aW9uIChjb25zdCBpZCkKCi0t
IGZtYXAKbWFwJyA6OiAoYSAtPiBiKSAtPiBPcHRpb24gYSAtPiBPcHRpb24gYgptYXAnIGYgbSA9
IGNhdGEgbSAoc29tZSAuIGYpIG5vbmUKCi0tICg+Pj0pCmZsYXRNYXAgOjogKGEgLT4gT3B0aW9u
IGIpIC0+IE9wdGlvbiBhIC0+IE9wdGlvbiBiCmZsYXRNYXAgZiBtID0gY2F0YSBtIGYgbm9uZQoK
LS0gZnJvbU1heWJlCmdldE9yRWxzZSA6OiBPcHRpb24gYSAtPiBhIC0+IGEKZ2V0T3JFbHNlID0g
ZmxpcCBjYXRhIGlkCgpmaWx0ZXIgOjogT3B0aW9uIGEgLT4gKGEgLT4gQm9vbCkgLT4gT3B0aW9u
IGEKZmlsdGVyIG0gcCA9IGNhdGEgbSAoXGEgLT4gaWYgcChhKSB0aGVuIHNvbWUgYSBlbHNlIG5v
bmUpIG5vbmUKCi0tIG1hcE1fCmZvcmVhY2ggOjogT3B0aW9uIGEgLT4gKGEgLT4gSU8gKCkpIC0+
IElPICgpCmZvcmVhY2ggbSBmID0gY2F0YSBtIGYgKHJldHVybiAoKSkKCi0tIGlzSnVzdAppc0Rl
ZmluZWQgOjogT3B0aW9uIGEgLT4gQm9vbAppc0RlZmluZWQgbSA9IGNhdGEgbSAoY29uc3QgVHJ1
ZSkgRmFsc2UKCi0tIGlzTm90aGluZwppc0VtcHR5IDo6IE9wdGlvbiBhIC0+IEJvb2wKaXNFbXB0
eSBtID0gY2F0YSBtIChjb25zdCBGYWxzZSkgVHJ1ZQoKLS0gV0FSTklORzogbm90IGRlZmluZWQg
Zm9yIE5vbmUKLS0gZnJvbUp1c3QKZ2V0IDo6IE9wdGlvbiBhIC0+IGEKZ2V0IG0gPSBjYXRhIG0g
aWQgKGVycm9yICJOb25lLmdldCIpCgotLSBtcGx1cwpvckVsc2UgOjogT3B0aW9uIGEgLT4gT3B0
aW9uIGEgLT4gT3B0aW9uIGEKb3JFbHNlIG0gbiA9IGNhdGEgbSAoY29uc3QgbSkgbgoKdG9MZWZ0
IDo6IE9wdGlvbiBhIC0+IHggLT4gRWl0aGVyIGEgeAp0b0xlZnQgbSB4ID0gY2F0YSBtIExlZnQg
KFJpZ2h0IHgpCgp0b1JpZ2h0IDo6IE9wdGlvbiBhIC0+IHggLT4gRWl0aGVyIHggYQp0b1JpZ2h0
IG0geCA9IGNhdGEgbSBSaWdodCAoTGVmdCB4KQoKLS0gbWF5YmVUb0xpc3QKdG9MaXN0IDo6IE9w
dGlvbiBhIC0+IFthXQp0b0xpc3QgbSA9IGNhdGEgbSAoOltdKSBbXQoKaXRlcmF0b3IgPSBlcnJv
ciAiYnp6dC4gVGhpcyBpcyBIYXNrZWxsIHNpbGx5LiIK
~~~

---
comments: true
date: 2008-01-01 08:03:31
layout: post
slug: doomsday-in-scala
title: Doomsday in Scala
wordpressid: 54
tags: Programming
---

Just for kicks, from [http://en.wikipedia.org/wiki/Doomsday_algorithm](http://en.wikipedia.org/wiki/Doomsday_algorithm)

~~~{.Scala}
sealed trait Day {
  def +(n: Int): Day
}
final case object Sunday extends Day {
  override def +(n: Int) = n % 7 match {
    case 0 => Sunday
    case 1 => Monday
    case 2 => Tuesday
    case 3 => Wednesday
    case 4 => Thursday
    case 5 => Friday
    case 6 => Saturday
  }
}
final case object Monday extends Day {
  override def +(n: Int) = n % 7 match {
    case 6 => Sunday
    case 0 => Monday
    case 1 => Tuesday
    case 2 => Wednesday
    case 3 => Thursday
    case 4 => Friday
    case 5 => Saturday
  }
}
final case object Tuesday extends Day {
  override def +(n: Int) = n % 7 match {
    case 5 => Sunday
    case 6 => Monday
    case 0 => Tuesday
    case 1 => Wednesday
    case 2 => Thursday
    case 3 => Friday
    case 4 => Saturday
  }
}
final case object Wednesday extends Day {
  override def +(n: Int) = n % 7 match {
    case 4 => Sunday
    case 5 => Monday
    case 6 => Tuesday
    case 0 => Wednesday
    case 1 => Thursday
    case 2 => Friday
    case 3 => Saturday
  }
}
final case object Thursday extends Day {
  override def +(n: Int) = n % 7 match {
    case 3 => Sunday
    case 4 => Monday
    case 5 => Tuesday
    case 6 => Wednesday
    case 0 => Thursday
    case 1 => Friday
    case 2 => Saturday
  }
}
final case object Friday extends Day {
  override def +(n: Int) = n % 7 match {
    case 2 => Sunday
    case 3 => Monday
    case 4 => Tuesday
    case 5 => Wednesday
    case 6 => Thursday
    case 0 => Friday
    case 1 => Saturday
  }
}
final case object Saturday extends Day {
  override def +(n: Int) = n % 7 match {
    case 1 => Sunday
    case 2 => Monday
    case 3 => Tuesday
    case 4 => Wednesday
    case 5 => Thursday
    case 6 => Friday
    case 7 => Saturday
  }
}

// From http://en.wikipedia.org/wiki/Doomsday_algorithm
object Doomsday {
  def anchor(y: Int) = y / 100 % 4 match {
    case 0 => Tuesday
    case 1 => Sunday
    case 2 => Friday
    case 3 => Wednesday
  }

  def doomsday(y: Int) = {
    val a = y % 100 / 12
    val b = y % 100 % 12
    val c = b / 4
    val d = a + b + c
    anchor(y) + d
  }
}
~~~



> scala> import Doomsday._
import Doomsday._

scala> doomsday(1966)
res0: Day = Monday

scala> doomsday(2008)
res1: Day = Friday

---
comments: true
date: 2008-10-13 18:29:41
layout: post
slug: does-scala-have-javas-ternary-operator
title: Does Scala have Java's ternary operator?
wordpressid: 375
tags: Programming
---

I hear this question a lot. Yes it does. Instead of ` c ? p : q`, it is written `if(c) p else q`.

This may not be preferable. Perhaps you'd like to write it using the same syntax as Java. Sadly, you can't. This is because `:` is not a valid identifier. Fear not, `|` is! Would you settle for this?

    
~~~{.Java}
c ? p | q
~~~



Then you'll need the following code. Notice the call-by-name (`=>`) annotations on the arguments. This evaluation strategy is required to correctly rewrite Java's ternary operator. This cannot be done in Java itself.


    
~~~{.Scala}
case class Bool(b: Boolean) {
  def ?[X](t: => X) = new {
    def |(f: => X) = if(b) t else f
  }
}

object Bool {
  implicit def BooleanBool(b: Boolean) = Bool(b)
}
~~~



Here is an example using the new operator that we just defined:


    
~~~{.Scala}
object T {
  val condition = true

  import Bool._

  // yay!
  val x = condition ? "yes" | "no"
}
~~~


Have fun ;)

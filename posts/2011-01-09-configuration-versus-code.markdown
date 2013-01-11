---
comments: true
date: 2011-01-09 07:28:34
layout: post
slug: configuration-versus-code
title: Configuration versus Code
wordpressid: 889
tags: Programming
---

Sometimes I'll be working away when a colleague will make some distinction between "that which is configuration" and "that which is code." In all cases, it turns out that this distinction is premised on a having a very narrow understanding of solving software problems. That can't be good.

When we write compositional code using techniques such as _referential transparency_, the distinction between configuration and code completely evaporates. I wish to emphasise this evaporation, so I will restate it: **it is not possible to determine which is configuration and which is code, except when using very sloppy problem solving habits, no matter how much we wish to save the thesis**. I made this emphasis because I have even seen people accept that the delineation disappears, then in the very next breath say something to the effect as if it were still there. Brains are fascinating machines. I digress.

Let us suppose a Haskell program, though, the language is unimportant to this point. We only need to examine the type signature:


    
~~~{.Haskell}
application :: IO ()
~~~
    



If we need some form of "configuration", such as a possible `String` value with a "default", we pass it as such:


    
~~~{.Haskell}
application :: Maybe String -> IO ()
~~~



Then the body of `application` will apply the default: `fromMaybe "default string"`. Easy right?

You might have multiple configuration values and not necessarily with the type `String`. Now of course, since we are using Haskell (and not XML!), we can give our configuration values a type! Here is an example:


    
~~~{.Haskell}
data Initialisation = Initialisation {
  ioWait :: Int
, performance :: Speed
, password :: String
}
~~~



then your application becomes:


    
~~~{.Haskell}
application :: Initialisation -> IO ()
~~~



With Haskell you also get record selector syntax with this data structure. This means that setting say, 2 of these values can be done with effort proportional to the size of 2. With languages such as Java, achieving this goal results in a quadratic blow-out, requiring n^2 methods with overloads where n is the number of record fields.

Now, notice how doing this is simply, **using a function**. Let's be clear, I am using a function, like any other, a function, code. It is not special and does not deserve any elevated status above other functions. No more useless ceremony here.

A good example of an application, that I use each day, is [xmonad](http://xmonad.org/), which I "configure" by writing Haskell. OK, now let's be honest, I just write a program and take advantage of the comprehensive libraries.

In some cases that make the distinction between configuration and code, it's often that an inappropriate language has been chosen to write code in, such as XML. There is a direct mapping between the XML and whatever programming language they are using. However, the XML will often exhibit properties that programming languages such as Haskell exhibit universally e.g. reordering statements does not affect the observable outcome of the program. In other words, this is just functional programming with XML (yeah, my thoughts exactly).

Any effort that delineates between configuration and code is nothing more than a very reliable indicator of sloppy thinking. Think again.

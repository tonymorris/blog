---
comments: true
date: 2006-11-16 15:36:30
layout: post
slug: revisiting-maybe-in-java
title: Revisiting Maybe in Java
wordpressid: 11
tags: Programming
---

I posted the link to [Maybe in Java](http://blog.tmorris.net/maybe-in-java) to programming.reddit.com and waited a good... hour or so, before giving up and going to bed. Actually, I happened to lose interest in posting it and by-chance, noticed my web browser still hadn't completed the request on the way to bed. Nevertheless, it seems my HTTP request managed to make it there and when I woke up the next morning, there were all sorts of comments floating around. A few in particular asking something along the lines of, "what exactly has been achieved?".

I will answer this question with "I have pointed out to Java programmers the concept of a very basic _algebraic data type_ and importantly, in their language". No mathematics, no type theory, nothing that will scare away your average J2EE Joey Jumper. I have also made a subliminal point of "hey! there are languages that already do this, only better!". In particular, I have not provided anything that you should all go out and start using on your next WebSphere-fronted, RDBMS-backed, one-trillion-gazillion LOC web application. It seems the following points were missed in my original writing:



	
  * Throwing an exception is one of our possible options for evaluation of a partial function in Java. Here are all our options available in Java: ... Emulate continuation passing style (CPS)

	
  * `data Maybe a = Just a | Nothing` (the Haskell equivalent i.e. omitting instances)

	
  * I’ll let your imagination run wild with possibilities from here :)



To remedy this situation, I have added further to the original Maybe type _which was intentionally left incomplete_, and it is **still not complete**. I hope this will help those who haven't made the leap to do so and those who have made the leap, to understand my objective in this writing.


    
~~~{.Java}
public abstract class Maybe<t> {
  private Maybe() {
  }

  public abstract <q> Q maybe(JustC<Q, T> jc, NothingC<q> nc);

  public static abstract class Nothing<t> extends Maybe<t> {
    private Nothing() {
    }
  }

  public static abstract class Just<t> extends Maybe<t> {
    private Just() {
    }

    public abstract T just();
  }

  public static <r> Maybe<r> _just(final R r) {
    return new Just<r>() {
      @Override
      public R just() {
        return r;
      }

      @Override
      public <q> Q maybe(final JustC<Q, R> jc, final NothingC<q> nc) {
        return jc.c(r);
      }
    };
  }

  public static <r> Maybe<r> _nothing() {
    return new Nothing<r>() {
      @Override
      public <q> Q maybe(final JustC<Q, R> jc, final NothingC<q> nc) {
        return nc.c();
      }
    };
  }
}

public interface JustC<Q, R> {
  Q c(R r);
}

public interface NothingC<q> {
  Q c();
}
~~~


Those of you familiar with the Visitor Design Pattern (or any other GoF design euphemism) will immediately recognise the modification - hence the title of the post :). Please feel free to replace identifiers with your preferred view of the world; continuation, quasi-continuation, visitor, whatever.

For those who _insist_ on returning `null` or throwing an exception/error, or more so, insist on failing to recognise the distinction, I have yielded to your pressure:


    
~~~{.Java}
public final class NullNothingC<t> implements NothingC<t> {
  public T c() {
    return null;
  }
}

public final class ErrorNothingC<t> implements NothingC<t> {
  public T c() {
    throw new Error();
  }
}
~~~

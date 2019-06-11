---
comments: true
date: 2008-07-25 15:26:27
layout: post
slug: actor-concurrency-for-java
title: Actor concurrency for Java
wordpressid: 98
tags: Programming
---

[Functional Java](http://functionaljava.org/) 2.8 contains a concurrency API that implements Actors as seen in Erlang and Scala. This allows a user to take advantage of multiple core machines in their Java code.

[Runar](http://apocalisp.wordpress.com/) has written articles explain how to use the API -- it's pretty darn easy for a client -- just don't look under the hood ;)




  
  * [Higher-Order Java Parallelism, Part 1: Parallel Strategies and the Callable Monad](http://apocalisp.wordpress.com/2008/06/18/parallel-strategies-and-the-callable-monad)

  
  * [Higher-Order Java Parallelism, Part 2: Parallel List Transformations](http://apocalisp.wordpress.com/2008/06/30/parallel-list-transformations/)



Here is an [example](http://projects.workingmouse.com/public/functionaljava/artifacts/2.8/demo/bgga/concurrent/Fibs.java) of a parallel fibonacci that uses a few hundred virtual threads (unlike [the ping/pong example](http://projects.workingmouse.com/public/functionaljava/artifacts/2.8/demo/bgga/concurrent/PingPong.java) that uses... wait for it... **millions** of virtual threads!). On my quad-core machine, the fibonacci computation speeds up by about 6 times (45 seconds serially to about 7 seconds when using actors).

[The example](http://projects.workingmouse.com/public/functionaljava/artifacts/2.8/demo/bgga/concurrent/Fibs.java) uses Java 7 BGGA syntax (imports omitted) and after compilation, runs fine on any 1.5 JVM. This example is [also available with Java 1.5 source code](http://projects.workingmouse.com/public/functionaljava/artifacts/2.8/demo/1.5/concurrent/Fibs.java) in the Functional Java release.


~~~{.Java}
/**
 * Parallel Fibonacci numbers.
 * Based on a Haskell example by Don Stewart.
 * Author: Runar
 */
public class Fibs {

  private static final int CUTOFF = 35;

  public static void main(final String[] args) throws Exception {
    if (args.length < 1)
      throw error("This program takes an argument: number_of_threads");

    final int threads = Integer.parseInt(args[0]);
    final ExecutorService pool = Executors.newFixedThreadPool(threads);
    final Strategy<unit> su = Strategy.executorStrategy(pool);
    final Strategy<Promise<integer>> spi = Strategy.executorStrategy(pool);

    final Actor<List<integer>> out = actor(su, { List<integer> fs => {
      int i = 0;
      for (List<integer> ns = fs; ns.isNotEmpty(); ns = ns.tail()) {
        System.out.println(MessageFormat.format("n={0}=>{1}", i, ns.head()));
        i++;
      }
      pool.shutdown();
    }});

    System.out.println("Calculating Fibonacci sequence in parallel...");

    final F<Integer, Promise<integer>> fib = { Integer n => (n < CUTOFF) ?
        promise(su, P.p(serialFib(n))) :
        fib.f(n - 1).bind(join(su, P1.curry(fib).f(n - 2)), { int a => { int b => a + b }} ) };

    join(su, fmap(Promise.<integer>sequence(su)).f(spi.parMap(fib).f(range(0, 46)))).to(out);
  }

  public static int serialFib(final int n) {
    if (n < 2)
      return n;
    else return serialFib(n - 1) + serialFib(n - 2);
  }
}
~~~

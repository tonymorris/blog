---
comments: true
date: 2011-01-09 09:44:27
layout: post
slug: scala-exercise-with-types-and-abstraction
title: Scala exercise with types and abstraction
wordpressid: 896
tags: Programming
---

Write an API for playing tic-tac-toe. There should be no side-effects (or variables), at all, for real. The `move` function will take a game state, a `Position` and it will return a data type that you write yourself.

The following functions (at least) must be supported:





  1. `move` (as mentioned)


  2. `whoseTurn` (returns which player's turn it is)


  3. `whoWon` (returns who won or if a draw)


  4. `playerAt` (returns which player, if any, is at a given position)



**Importantly**, the following must be true:





  1. It is a **compile-time error** to call `move` or `whoseTurn` on a game that has been completed


  2. It is a **compile-time error** to call `whoWon` on a game that has **not** been completed


  3. The `playerAt` function must be supported on both complete and in-play games



Good luck and may the types be with you.

---
comments: true
date: 2011-03-05 13:39:16
layout: post
slug: understanding-practical-api-design-static-typing-and-functional-programming
title: Understanding Practical API Design, Static Typing and Functional Programming
wordpressid: 943
tags: Programming
---

_...and how a marriage to a particular programming language is only indirectly and barely relevant_

I was really tempted to title this post, "What to Solve Before Expecting Me to Take Your Opinions of Static Typing Seriously", however, I figured this would be a bit too controversial and might detract from the points I wish to make. Nevertheless, I just want to make note, I think it is a very appropriate title.

The reason I think it's a very appropriate title, is because of certain events that have happened recently. I teach advanced programming techniques to programmers. I do this voluntarily for the most part, and I occasionally deliver guest lectures to universities and other sporadic occurrences. I also teach at my place of employment, where I use these techniques for product development. I used to do university lecturing, until I came to the conclusion that the tertiary institution is in direct contention with the goal of education; the latter of which is important to me.

I have constructed the course material myself, predicting what would be useful, too difficult or too easy and so on and revising over time as these predictions fell out of place. Recently I set a task, predicted how difficult it would be, then was astonished to find that it appears to be _significantly more difficult than I had originally predicted_. I'm still not sure what is going on here, however, I think there are some lessons to be taken. One of which is a lesson about approaches to teaching advanced concepts of programming -- something I am constantly learning about (and yearning for more solid research and experimental results!).

I asked students to write an API for the game tic-tac-toe. No need for the computer to tactically play tic-tac-toe -- just an API that you can use to model the game itself. You can use any programming language you like, however, I think you will find certain environments to be lacking in the available tools, so I will guide you so that you're not off somewhere "shaving yaks" so to speak.

If I'd left the requirement there, I can predict what I would have ended up with. Probably something similar to the API that I used to support at L3 for IBM where the number of bugs coming in was at a rate faster than I could fix them -- and we don't want that. Worse still, every time I fixed one bug gave rise to several new ones, no matter what I did! The whole point of this exercise is to avoid this scenario, once and for all, and without all that nonsense that gives false promise to deliver on this objective (Agile, XP, Scrum and all that silliness).

So I set some rules, but without further explanation of why these rules existed:



	
  * If you write a function, I must be able to call it with the same arguments and always get the same results, forever.

	
  * If I, as a client of your API, call one of your functions, I should always get a sensible result. Not `null` or an exception or other backdoors that cause the death of millions of kittens worldwide.

	
  * If I call `move` on a tic-tac-toe board, but the game has finished, I should get a **compile-time type-error**. In other words, calling `move` on inappropriate game states (i.e. move doesn't make sense) is disallowed by the types.

	
  * If I call `takeMoveBack` on a tic-tac-toe board, but no moves have yet been made, I get a **compile-time type-error**.

	
  * If I call `whoWonOrDraw` on a tic-tac-toe board, but the game hasn't yet finished, I get a **compile-time type-error**.

	
  * I should be able to call various functions on a game board that is in any state of play e.g. `isPositionOccupied` works for in-play and completed games.

        
  * It is not possible to play out of turn.



Now, why these rules? Well, because if you can achieve the goal of enforcing these rules, then the next phone call that I get in L3 support from an upset client, I can be guaranteed that one of the following are true:

	
  * I have a bug in my code, in which case, the sooner the call, the better! ...unless of course, fixing the bug results in only more bugs -- but we have avoided that possibility -- hopefully you can see why.

	
  * The client has misused the API and circumvented the type system. The client has used `null`, thrown an exception or performed a side-effect within the API or perhaps even used such things as Java reflection or even type-casting or type-casing. Unfortunately, in some environments, there's not much I can do about enforcing that except impose a de facto rule where you assume non-existence of these possibilities (Just don't do that!). Hopefully you haven't yet jumped to the conclusion that any of these things are necessary or even useful -- they aren't.

	
  * The client is simply mistaken about the merits of their complaint.


That's it. There is nothing more to add to the list. **Importantly, this list is significantly shorter than the list that I once had when supporting a typical Java application in IBM L3 support.** How did I achieve this narrow list of possibilities before the phone even rang?

I have actually solved this problem using various programming languages:


	
        
  * Haskell

	
  * Scala

	
  * C#

	
  * Java


In order to take the emphasis off programming-language-centric issues, I will focus now on the Java solution (the most challenging environment in which to achieve the goal) and then I  am going to invite you to conduct a thought experiment.

Let's take a look at [the javadoc for the Java solution](http://dl.dropbox.com/u/7810909/TicTacToe/javadoc/index.html). Ignore the `Main` class for now, which simply gives you a 2-player console application that uses the API (feel free to run it!) -- it depends on the rest of the API and so could be deleted without breaking anything.

If I asked you to be the most malicious user you can be, so long as you followed the rules (which I will assume you have done from here on), I want you to get an instance of the `FinishedBoard` class. If you rang me up in L3 support, even hiding your malicious intent from me, and said you had such an instance, then _I am absolutely guaranteed that you obtained that instance by playing a legitimate game of tic-tac-toe and ended up with a game that is in the position of a player winning or a draw_. Consider the implications of this for a moment.

For another example, suppose you rang and said that you called the `move` function and when you ran it, something-or-rather happened at run-time, _I can be guaranteed that you called that function on a game that was in-play and so had the ability to move, since otherwise it would not have compiled, let alone run_.

There are many more examples of these types of guarantees -- invariants that I am certain have been met before I even start listening to you on the phone. I think this is an enormous advantage to real-world software tasks, don't you? And all this done with Java and its overwhelmingly impractical type system. How about that!?

Now, producing an API of this nature seems to be more difficult for programmers than I originally predicted. Why is this? I can only conjecture and given my already-disproportionate prediction, I am hesitant to do so. Nevertheless, what you are looking at is _an extremely robust API for a relatively trivial problem_. This API robustness was achieved by exploiting techniques available from static typing and functional programming.

So let's summarise. A robust API for a trivial game was written using several programming languages in such a way that appears to be difficult for many programmers to reproduce and using techniques such as exploiting static typing and functional programming. This was even done with a popular programming language that is not particularly amenable to achieving this degree of robustness.

In other words, many programmers have difficulty solving a trivial problem using techniques that many programmers are compelled to offer their comment on -- no wonder there is a lot of outrageous hype! On the topic of why I expect you to be able to solve this problem before I take you seriously -- it's not because I have high standards, they're incredibly low -- it's just that while these standards are very low, they are rarely satisfied. This is not a high-horse-motivated rant (as many insecure people might hastily conclude), the point I am making here is that I think it's important to set standards of scepticism -- maybe you should too!

It's not just support issues where you will see an advantage to this programming technique. You might be writing an API for the guy sitting next to you. Those types are machine-checked documentation -- he won't have to keep bothering you about which function to call when -- it's obvious, since it's specified in the type! You might even be your own client -- suppose you hit a bug in your own code -- you can rule out a huge number of possibilities of where that bug is, before you even start looking for it.

All these advantages of robust API design for less expense than the contrary and yet robust API design seems to be overwhelmingly rare. I am left only with questions, aren't you? Come on guys, we can do a whole lot better. It's an invitation!

It's possible to take this particular API problem further using a language such as Agda or Coq and enforce the invariant that it's not possible to make a move to a position on a board if that position has already been moved to. I know of one person who is attempting to achieve this. Godspeed.

I haven't shown you the source to any of these solutions because I figure you might want to take a crack at it yourself. Give it a go! If you really want the source let me know and I'll send you a link.

Hopefully this helps!

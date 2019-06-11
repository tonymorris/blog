---
comments: true
date: 2006-12-10 21:19:33
layout: post
slug: software-requirements-do-not-change
title: Software requirements do not change
wordpressid: 15
tags: Programming
---

There is quite a prolific myth that seems to have a grip on the collective software development industry. Often in communication with another software developer, the notion of a "change in requirement" will surface and I am forced to remind that developer that no such thing ever occurs - that the internalisation of this logical fallacy is in fact, quite detrimental to the purpose of creating software, fulfilling software requirements and should be abandoned. Since no such thing as a software requirement change ever occurs, the remainder of the conversation is invalidated making communication very difficult. Who's responsibility is it to establish a sound knowledge in the fundamental concepts of their selected profession? Not mine - I am forced to cease the conversation. After all, I don't explain to my doctor what my problem is - only my symptoms - hence, I am not a doctor. But I'll bet doctors speak in a "language" that is beyond my capability of comprehending (though I have picked up a term or two having lived with a midwife for years :)). We software developers should be doing the same, but it does not seem to be the case.

Software is a function from some given inputs to some given outputs (a lambda if you will) and importantly, given some inputs, the same output should always result, **always**. It should not return some other result; it should not format your hard disk every second time and it shouldn't cause the universe to implode or anything crazy like that - just consistently return the same value for its given inputs. This property is called _referential transparency_. Let's look at a specific piece of software called 'touch'. It takes as input:



	
  1. a file system

	
  2. a list of characters


It provides one output - a new file system. If a file of the given name exists on the given file system, return the new file system with that file having an updated modified time stamp, otherwise, return the new file system with an additional empty file of the given name. That's it - nothing more to it. We might even express this software more formally as follows:

    
~~~{.Haskell}
-- "Given a FileSystem,
--   then a list of characters (Char),
--   return a new FileSystem."
-- :: means 'is of type'
touch :: FileSystem -> [Char] -> FileSystem
touch fs filename = if fs `contains` filename
  then fs `updateTimestamp` filename
    else fs `addEmptyFile` filename
~~~


The 'touch' software is so fundamental, that most reasonable operating systems provide it without any explicit user intervention - try running it and see.

While you are free to come up with all sorts of wild and complex examples of software, I'm going to simplify a little with a new example. Suppose a client approaches you, a software developer, on Monday, with the following requirement; "given two integers, sum them, multiply by 4 and give me the result." Here is the software written in Java:

    
~~~{.Java}
int software(int a, int b) {
    return (a + b) * 4;
}
~~~


Easy peasey! On a side note, write it in Haskell, then play 'spot the difference' with the client problem statement and the code. Fun hey? :)

If the client approaches you on Tuesday with the following, "given two integers, subtract the first from the last. multiply by 4 and give me the result.", then it is **imperative** to understand that this is **a new requirement**. A software requirement holds from inception until the collapse of the universe. You might argue, "but the old one and the same one look almost the same", in which case, feel free to write the second one by applying your favourite delta compression algorithm, but by no means should the former be _replaced_ by the latter. You might also argue, "but the client said it is a change". Well I hope my doctor doesn't follow any advice that I give him on my condition at the time since I am not well versed in medical practice. I do not say, "I have a headache, I need to fix it with these drugs" or if I did, the doctor, being aware of my naivety, would ignore my advice and only consider the symptom (I would hope!). Likewise, clients do not project their false view of software into their final product and instead, trust the better judgment of a more trained practitioner (ideally).

If we take the common (not that I accept its legitimacy in this form) approach of unit testing and I write `assert(software(42, 43) == 340)`, then this statement should hold forever. If it does not, then you have broken your contract to all clients of your software. The typical refutation to this assertion involves the developer ascertaining that no software clients have ever existed and so it is safe to continue. For example, if there is even a slight suspicion of a client, that developer might first deduce that if there is, then those client(s) are in the room. Then that developer asks all people in the room to commit their code so that she can search for occurrences of its use. If none are found, the software is removed, code committed and all persons in the room are instructed to update. Otherwise, if it is found that one or more clients exist, manual contingency measures are engaged.

The regularity of the occurrence of no clients bound to your software is often over-estimated. Just take a look at [the mess that Microsoft has itself in with an attempt to 'separate out the components of Windows'](http://www.drizzle.com/~lettvin/2006/11/windows-shutdown-crapfest.html) or any of the other countless examples that surround us. You might say, "but I am a consultant with only one client". This is not true; in fact, the client/provider relationship can often be self-referential. It is _entirely coincidental_ that no clients have bound to your software, but rarely is it so.

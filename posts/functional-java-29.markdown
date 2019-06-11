---
comments: true
date: 2008-08-05 11:33:43
layout: post
slug: functional-java-29
title: Functional Java 2.9
wordpressid: 193
tags: Programming
---

**Patient**: I have 6 potentially failing methods (a, b, c, d, e, f) and if one of those fails, I want to cease execution and return that failure, otherwise continue execution.

**Doctor**: What do they return if they succeed?

**Patient**: Nothing, they side-effect

**Doctor**: eek! OK, let's see what we can do? What happens after you've completed this computation?

**Patient**: Well, it gets a bit hairier you see. Then I have 3 more potentially failing computations (g, h, i) and if any of those fail (or the original failed), then I also want to fail, however, I want to _keep_ the errors in these last three computations.

**Doctor**: So let's get this right, you perform all of the latter three computations regardless of their outcome and you only succeed if all nine computations succeed and you fail otherwise?

**Patient**: Yes, that's right and...

**Doctor**: And for whatever silly reason, you're using Java.

**Patient**: _cowers_; er yeah.

**Doctor**: Well, I've told you about that, haven't I?

**Patient**: _cowers more_; yes you have but...

**Doctor**: So, if any of the first six computations fail, then you check the latter three for failures as well. These latter three are side-effecting, void return type, as well aren't they?

**Patient**: Yes...

**Doctor**: If the first six succeed, you perform the latter three computations anyway, accumulating potential failures.

**Patient**: Right, exactly

**Doctor**: And just as an interesting observation, you will have at most, four errors and possibly none in the event of all nine succeeding.

**Patient**: Umm yeah, I hadn't thought of it that way.

**Doctor**: _smiles_


    
~~~{.Java}
Validation<Throwable, Unit> a;
Validation<Throwable, Unit> b;
Validation<Throwable, Unit> c;
Validation<Throwable, Unit> d;
Validation<Throwable, Unit> e;
Validation<Throwable, Unit> f;
////
Validation<Throwable, Unit> g;
Validation<Throwable, Unit> h;
Validation<Throwable, Unit> i;

Validation<Throwable, Unit> first() {
  return a.sequence(b).
          sequence(c).
          sequence(d).
          sequence(e).
          sequence(f);
}

Option<NonEmptyList<throwable>> second() {
  return first().nel().accumulate(Semigroup.<throwable>nonEmptyListSemigroup(),
          g.nel(),
          h.nel(),
          i.nel());
}
~~~



**Doctor**: Now, this is the best that Java can do at solving this very popular request of yours, but come and see me when you're ready to upgrade your tools...

**Patient**: Thanks Doc! I will!

**Doctor**: _outward smile, inward scepticism_; In the meantime, I will prescribe you with [Functional Java 2.9](http://functionaljava.org/) which is only going to work if you exercise _at least some_ amount of intellectual discipline. You will need it for the solution above.

**Doctor**: On your way then.

_Update: Functional Java 2.10 includes `Validation.sequence`._

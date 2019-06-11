---
comments: true
date: 2010-11-24 10:57:15
layout: post
slug: dear-java-library-guy
title: Dear Java library guy
wordpressid: 849
tags: Programming
---

~~~{.Java}
import java.io.File;
import java.util.Calendar;

import static java.util.Calendar.DAY_OF_WEEK;
import static java.util.Calendar.THURSDAY;
import static java.util.Calendar.TUESDAY;
import static java.util.Calendar.WEDNESDAY;

public class ThreeAdder {
  // Convenience method to add the two given numbers, then adds 3.
  // It's really convenient, promise.
  // It even has tests and they passed! See below.
  public static int addThen3(int a, int b) {
    if(new File("/etc/passwd").exists() && a < 100) {
      return a + b + 3;
    } else if (b == 2) {
      int day = Calendar.getInstance().get(DAY_OF_WEEK);
      if (day == TUESDAY || day == WEDNESDAY || day == THURSDAY)
        return 5 + a;
      else
        return 9;
    } else if(a == 0) {
      return b + 3;
    } else if(b == 0) {
      return a + 3;
    } else {
      return 8;
    }
  }

  public static <a> String assertEq(String name, A x, A y) {
    return name + (x.equals(y) ?
      " [PASSED]" :
      " [FAILED] {" + x + "}  {" + y + '}');
  }

  public static void main(String[] args) {
    String[] out = {
        assertEq("adding to zero", addThen3(0, 0), 3)
    ,   assertEq("adding to zero", addThen3(0, 4), 7)
    ,   assertEq("adding to zero", addThen3(4, 0), 7)
    ,   assertEq("adding to zero", addThen3(1, 0), 4)
    ,   assertEq("adding to zero", addThen3(0, 1), 4)
    ,   assertEq("adding to zero", addThen3(0, 1), 4)
    ,   assertEq("small numbers",  addThen3(2, 4), 9)
    ,   assertEq("small numbers",  addThen3(2, 3), 8)
    ,   assertEq("small numbers",  addThen3(3, 3), 9)
    ,   assertEq("small numbers",  addThen3(3, 2), 8)
    };

    for(String o : out) {
      System.out.println(o);
    }
  }
}
~~~



No.

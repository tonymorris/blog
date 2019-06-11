---
comments: true
date: 2008-08-13 09:34:04
layout: post
slug: java-interop-errata
title: Java interop errata
wordpressid: 213
tags: Programming
---

There are a few possibilities for improvement on this [Scala Swing Example](http://blog.thinkrelevance.com/2008/8/12/java-next-2-java-interop).




  
  * Removed unnecessary semi-colons

  
  * Inlined import

  
  * Removed `=` in main declaration (strongly advised on functions returning `Unit` and especially more for `main`)

  
  * Removed a few unnecessary parentheses




    
~~~{.Scala}
import javax.swing._
import java.awt.event.{ActionEvent, ActionListener}

object HelloWorld extends JFrame("Hello Swing") {
  def showButtonMessage(msg: String)  =
    JOptionPane.showMessageDialog(null, String.format("""<html>Hello from <b>Scala</b>. Button %s pressed""", Array(msg)));

  def main(args: Array[String]) {
    setDefaultCloseOperation(JFrame.EXIT_ON_CLOSE)
    val button = new JButton("Click Me")
    button.addActionListener((e:ActionEvent) => showButtonMessage(e.getActionCommand.toString))
    getContentPane add button
    pack
    setVisible(true)
  }

  implicit def actionPerformedWrapper(func: (ActionEvent) => Unit) = new ActionListener {
    def actionPerformed(e:ActionEvent) = func(e)
  }
}
~~~

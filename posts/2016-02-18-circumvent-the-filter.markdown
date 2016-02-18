---
comments: true
date: 2016-02-11 19:00:0
layout: post
slug: circumventing-the-filter
title: Circumventing Australia's Internet Filtering Mechanism
tags: Programming, Internet Filter, Law
---

On 31 December 2007 the Telecommunications Minister the Australian government
announced that Australia would be introducing mandatory internet filtering. 
The only implementation of this idea, until now, has been the ridiculed
[blocking of 250000 websites by the Australian Securities and Investments Commission (ASIC) in April 2013](http://www.abc.net.au/news/2014-08-27/asic-accidentally-blocked-250,000-websites-ip-address/5701734)
under *section 313 of Australia's Telecommunications Act*. The block was
intended for only two websites engaged in fraudulent activity. In a subsequent
inquiry, ASIC commented that "99.6 per cent of those sites contained no
substantive content."

The reason this occurred is because the block performed filtering of an internet address called an *IP address*, however, multiple unrelated websites can share an IP address. In doing implementing this type of filtering, ASIC blocked an unrelated 250,000 unrelated websites. Clearly, a blocking mechanism cannot be implement this way and a repeat has not occurred since. Nevertheless, such profound eyeroll-inducing incompetence has become a recent trend of the (federal) Australian government. A repeat of some such stupidity is imminent.

Australian politicians have been banging on about internet filtering for quite some time now. Unfortunately, those same politicians seek their "expert advice" from such (dopey) organisations as the Australian Federal Police. Australia does indeed fund a significant research organisation of actual experts in this field, however, the advice that might be provided by such experts (unanimously) would not be conducive to the agenda, and so it is largely ignored.

In June 2015, a bill called [s.115a of the Copyright Amendment (Online Infringement) Bill 2015](https://www.comlaw.gov.au/Details/C2015B00052) was passed in Senate. This bill gives holders of copyright (e.g. television, movies, music) an ability to apply to our Federal Court to have piracy-related websites blocked from being used in Australia. At the forefront of this bill is an effort to block [The Pirate Bay](http://thepiratebay.se/). Even though this website does not host material that infringes copyright, it contains links to copyrighted material. Despite the fact that Facebook directly hosts infringing copyrighted material, it is websites like The Pirate Bay that have become the centre of attention.

Since we know that IP-address filtering does not work, and the all parties involved in the implementation of this fascination with website blocking are incompetent, we can easily predict their next move. It is predicted that a technique called **DNS poisoning** will be implemented and of course, this will also not work, because nothing will. Today, it was announced that [the first application for implementing this blocking (under the aforementioned bill) has been made to our Federal Court](http://www.abc.net.au/news/2016-02-18/village-roadshow-launches-legal-action-to-block-piracy-website/7176688). As you can see in the ABC article, it is speculated that DNS poisoning will be the method of implementation for thhis block.

In this article, I will show you how to circumvent this block. There are a few different ways of achieving this. It is not illegal for me to tell you how to do this, and it is not illegal for you to implement this block. I achieve no monetary gain by writing this article. My single motivation is freedom for myself, and for others. We can all do good things, despite the numpties numptying about in federal government. Aside, if such a block were implemented 10 years ago, it would not have affected me, as my network already incidentally avoids the mechanism for implementation. I will show you how.

First let's understand a little bit about "DNS." Suppose you have a broadband connection, such as ADSL or Cable. You might have this service with a provider such as Telstra or Optus. In your property somewhere, you will have a "ADSL modem" or a "cable modem" -- sometimes called a modem/router. When you go to a website with a name such as `google.com` or `facebook.com`, your computer asks your modem/router to *provide an internet address for that name*. Your modem/router will have a setting for what to do when this happens. Your modem/router will have a setting so that it probably then asks the same question from your service provider (such as Telstra or Optus). When your service provider responds to your modem/router with the address for your desired website, that address is then sent to your computer, so now your computer knows which website address to go to. This happens every time you type a website name into your web browser. DNS is all about a **Naming System** for all this to occur. That's all you need to know to now understand DNS poisoning.

The method of DNS poisoning is simply this. When your service provider (Telstra, Optus, whatever) sends back the website address for your website name, there is a tiny little itty-bitty dunce from our government standing on their pedestal saying the following

> Right that's it Telstra! Stop telling your customer's modem/router the correct address for that website name! And you too Optus, and all of you STOP IT RIGHT NOW"

Instead, your modem/router will be told the incorrect answer for a website name such as `thepiratebay.se` and now you are blocked from using that website.

To circumvent this 

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

The reason this occurred is because the block performed filtering of an internet
address called an *IP address*, however, multiple unrelated websites can share
an IP address. In doing implementing this type of filtering, ASIC blocked an
unrelated 250,000 unrelated websites. Clearly, a blocking mechanism cannot be
implement this way and a repeat has not occurred since. Nevertheless, such
profound eyeroll-inducing incompetence has become a recent trend of the
(federal) Australian government. A repeat of some such stupidity is imminent.

Australian politicians have been banging on about internet filtering for quite
some time now. Unfortunately, those same politicians seek their "expert advice"
from such (dopey) organisations as the Australian Federal Police. Australia does
indeed fund a significant research organisation of actual experts in this field,
however, the advice that might be provided by such experts (unanimously) would
not be conducive to the agenda, and so it is largely ignored.

In June 2015, a bill called [s.115a of the Copyright Amendment (Online Infringement) Bill 2015](https://www.comlaw.gov.au/Details/C2015B00052)
was passed in Senate. This bill gives holders of copyright (e.g. television,
movies, music) an ability to apply to our Federal Court to have piracy-related
websites blocked from being used in Australia. At the forefront of this bill is
an effort to block [The Pirate Bay](http://thepiratebay.se/). Even though this
website does not host material that infringes copyright, it contains links to
copyrighted material. Despite the fact that Facebook directly hosts infringing
copyrighted material, it is websites like The Pirate Bay that have become the
centre of attention.

Since we know that IP-address filtering does not work, and the all parties
involved in the implementation of this fascination with website blocking are
incompetent, we can easily predict their next move. It is predicted that a
technique called **DNS poisoning** will be implemented and of course, this will
also not work, because nothing will. Today, it was announced that [the first application for implementing this blocking (under the aforementioned bill) has been made to our Federal Court](http://www.abc.net.au/news/2016-02-18/village-roadshow-launches-legal-action-to-block-piracy-website/7176688). As you can see in the
ABC article, it is speculated that DNS poisoning will be the method of
implementation for this block.

In this article, I will show you how to circumvent this block. There are a few
different ways of achieving this. It is not illegal for me to tell you how to do
this, and it is not illegal for you to implement this block. I achieve no
monetary gain by writing this article. My single motivation is freedom for
myself, and for others. We can all do good things, despite the numpties
numptying about in federal government. Aside, if such a block were implemented
10 years ago, it would not have affected me, as my network already incidentally
avoids the mechanism for implementation. I will show you how.

First let's understand a little bit about "DNS." Suppose you have a broadband
connection, such as ADSL or Cable. You might have this service with a provider
such as Telstra or Optus. In your property somewhere, you will have a "ADSL
modem" or a "cable modem" -- sometimes called a modem/router. When you go to a
website with a name such as `google.com` or `facebook.com`, your computer asks
your modem/router to *provide an internet address for that name*. Your
modem/router will have a setting for what to do when this happens. Your
modem/router will have a setting so that it probably then asks the same question
from your service provider (such as Telstra or Optus). When your service
provider responds to your modem/router with the address for your desired
website, that address is then sent to your computer, so now your computer knows
which website address to go to. This happens every time you type a website name
into your web browser. DNS is all about a **Naming System** for all this to
occur. That's all you need to know to now understand DNS poisoning.

The method of DNS poisoning is simply this. When your service provider (Telstra,
Optus, whatever) sends back the website address for your website name, there is
a tiny dunce from our government standing on their legislative pedestal
proclaiming the following:

> Right that's it Telstra! Stop telling your customer's modem/router the correct
address for that website name! And you too Optus, and all of you STOP IT RIGHT
NOW or else."

Unfortunately, under our new legislation, your service provider, and all
Australian service providers, must comply with this demand. Instead of normal
operation, your modem/router will be told the incorrect answer for a website
name that you type into your web browser, such as `thepiratebay.se` and now you
are blocked from using that website.

However, not all service providers must comply with this request. Specifically,
not those located outside of Australia. Other DNS answers for website names will
still be providing the correct address. For example, Google has a DNS service
that provides the correct answer. Many others do too.

You are blocked because your modem/router has a setting that says, "when looking
up the address for a website name, use my Australian service provider to look it
up." You can circumvent this by:

1. Changing the setting on your modem/router to use a different DNS service than
the automatic one provided by your service provider.

2. *Add a setting on your computer* so that a specific website name answers with
a specific address. This will only work on that one computer.

3. There are other mechanisms for circumvention, but they are a bit more
complicated, so I will leave those out.

----

### Changing the setting on your modem/router

You will need to go to the administration interface for your router. I
understand this might not be easy for some. However, it is often as simple as
typing `192.168.0.1` or `192.168.1.1` into your web browser. If you are unsure
how to do this step, perhaps ask a friend for some help. It is actually quite
easy, so give it a try, but if you get stuck, seek out help!

Once you are there, you need to change a setting to "use the given DNS servers",
then enter in the address of a server that does not fall under Australia's
legislation, such as Google. Sometimes the setting will have a "primary" and a
"secondary" DNS server setting. The address for Google's servers are `8.8.8.8`
and `8.8.8.4`. Here is how it looks on a Netgear modem/router that I had lying
around:

![Netgear DNS settings](https://i.imgur.com/osv3MhS.png)

Save the settings and once your modem/router is no longer using a DNS service
from within Australia, that's it, you have circumvented Australia's internet
filter. You are no longer DNS-poisoned and all the computers at your house will
now receive the correct answer for a website name.

[Here is an example of a similarly oppressed society in Turkey](http://mic.com/articles/85987/turkish-protesters-are-spray-painting-8-8-8-8-and-8-8-4-4-on-walls-here-s-what-it-means#.pPjaa8I1v).
They spray painted buildings with the Google settings for a DNS service to
circumvent their government's foolish internet filtering service.

![8.8.8.8 in Turkey](http://i.imgur.com/N9BoYDy.jpg)

You might not wish to use Google's DNS service. There are other services outside
of Australia; for example, OpenDNS. The addresses for this are: `208.67.222.222`
and `208.67.220.220`. You might consider looking into this some more and finding
a DNS service (outside of Australia) that you are comfortable with.

Wait is it that easy? Yes, it is. So why aren't more people making this simple
change to their home internet settings? Yes, exactly. Now hurry up. Freedom is
ours and you are a part of that.

### Changing the setting on your specific computer for a specific website

The website address for the name `thepiratebay.se` at this moment is
`141.101.118.194`. If you typed `thepiratebay.se` into your web browser, your
computer would ask your modem/router for the website address for the name
`thepiratebay.se` and then your modem/router will ask your internet service
provider (e.g. Telstra) for the address and it will respond with
`141.101.118.194` and now your web browser will go to that address. If our
government has our way, Telstra will give you an *incorrect website address*
instead.

However, you can tell your computer not to use your modem/router to find the
website address for `thepiratebay.se` but instead, to make it always be
`141.101.118.194`. You do this by *editing your hosts file*.

Open up your hosts file on your computer with a text editor, such as Notepad on
Windows, or TextEdit on Mac OSX. Your hosts file is located:

* On Windows: `%SystemRoot%\System32\drivers\etc\hosts`

* On Mac OSX: /private/etc/hosts

With your text editor, select open a file and then type in the file location as
above. You may have problems with editing or opening this file because of your
computer's security. If that is the case, please ask a friend (or me) to help
you. It's usually a matter of using the administrator account of the computer to
successfully edit the file.

With the file open, add the following line at the end of the file:

`141.101.118.194    thepiratebay.se`

Save the file and close the text editor.

That's it, you are done. Your computer will now have the correct answer for the
website name `thepiratebay.se`. You might consider doing this for other website
names that you are concerned might fall under Australia's incompetent internet
filtering regime, such as `facebook.com`. Just kidding, that will never happen.
Ever asked yourself why? I digress.

----

Hopefully this is a reasonable start to your proactive pursuit of freedom. There
is a lot more if it is interesting to you. Seek it out. Experts in the field
enjoy helping you to protect yourself and in doing so, protect their own freedom
too.

I hope to leave a better world to my children than the one into which I was
born. Join me.

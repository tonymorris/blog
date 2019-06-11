---
title: Writing for The Gap Chess Club
date: 2018-03-28
authors: tmorris
---

## Introduction

This blog is built using [Hakyll](https://jaspervdj.be/hakyll/), a static site generator written in Haskell.

If you haven't used Hakyll before, this will walk you through what you need to know to get going.

If you have used Hakyll before, there are some customizations that we're using that you should probably know about - so keep reading.

## How to write a post

The first step is to checkout the from [github](https://github.com/thegapchessclub/thegapchessclub.org.au).

The blog posts are all in the `posts` directory, so that's where we add new ones.

If you're working on a draft that you want to share with people in order to get feedback, you can put it in the `drafts` directory.
It will be treated just like a regular post, but it won't be linked from any of the other pages.

Similarly, you can add content to the `links` directory if you have information that you want to share with people that would be noisy or off-topic if it were linked from the other pages.

In either case, the content will be viewable by anyone who has the URL of the content (or anyone clicking through the github repository for the blog), but otherwise won't be discoverable by people clicking around on the blog.

They are written using [Pandoc's Markdown](http://pandoc.org/MANUAL.html#pandocs-markdown) syntax, which is worth getting familiar with.

There is also support for `LaTeX`.
This can include the use of arbitrary packages, but there are currently a few steps involved in adding new packages.
I'll work on making this a bit smoother shortly.

### The property block for blog posts

The posts need to start with a property block, that looks like this:
```
---
title: Chess is best
date: 2017-04-01
authors: tmorris
---
```

The `title` and `date` fields are mandatory.
The blog will fail to build if you leave them out, but it'll let you know why.

The `authors` field is optional.
If it is set, it should be set to a comma-separated list of nicknames of the authors.

If the field is present, the post will be linked from the main page for each author.
The post will also have footer entries which link to that main pages of each author along with a small blurb.

## How to test out changes locally

There are two directories in the blog repository.

The `generator` directory has the site generator, and the `content` directory has the site content.

If you have Nix installed, you can use
```
nix-build release.nix -A generator
```
to build the site generator, which will be available in `result/bin/site`.

After that you can head to the `content` directory and run
```
../result/bin/site build
```
to build the site in `_site`, or
```
../result/bin/site watch
```
to set up a preview server on `localhost:8000` that will update as you work on your posts.

## Adding things

### Adding a new author

There are three things to do to add a new author to the blog.

At the moment these are pretty free-form, but in the future we might add more structure for standard sections / features - like photos of the team members, etc...

#### 1. Write an author page

Write a page about the author at `./people/<author-nickname>.md` with the following property block at the start of the document:
```
---
title: <Author name here>
---
```

#### 2. Write a snippet for the page listing members

This goes in `./snippets/people/<author-nickname>/page.md` and requires the following property block at the start of the document:
```
---
title: <Author name here>
order: <What position on the page the blurb will appear at>
---
```

The blurbs will be sorted in ascending order by the numerical value of the `order` property.
It is probably worth using the highest number not already taken.

#### 3. Write a snippet for blog posts

This goes in `./snippets/people/<author-nickname>/posts.md` and requires the following property block at the start of the document:
```
---
title: <Author name here>
---
```

### Adding a new `LaTeX` package

At the moment this is a bit manual.

The blog currently makes the `prftree` package available.

If you want to add a new LaTeX package, you can add it to the lists of packages in `default.nix` and `src/Site.hs` that already contain `prftree`.

This will be made a bit easier later on.

### Adding other content

Anything in the `./share` directory will be served up as static content.

## Conclusion

Blog early and often.
Just have fun with it.

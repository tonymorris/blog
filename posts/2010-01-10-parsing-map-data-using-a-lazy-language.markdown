---
comments: true
date: 2010-01-10 10:25:25
layout: post
slug: parsing-map-data-using-a-lazy-language
title: Parsing map data using a lazy language
wordpressid: 617
tags: Mapping, Programming
---

Haskell is a pure lazy programming language. The laziness of Haskell allows certain performance improvements without sacrificing compositional program properties. I have recently written parsers for XML map data formats that allow a user to "read a data file into a collection of immutable objects." If I told you I used the parser library to "read in" a 140GB map data file and you're not familiar with a lazy language, you might have asked how I did this within the constraints of memory requirements. Easy of course; I used a lazy language. The implications of a lazy (and therefore, pure) language are widely misunderstood, so I say "easy" wishing it really was easy for all people, but I know it isn't (keep practicing!).

[HXT](http://www.haskell.org/haskellwiki/HXT) is a parsing library for XML that is based on [Hughes' arrows](http://www.cs.chalmers.se/~rjmh/afp-arrows.pdf) and allows a user to piece together their own specific XML parser. I used it to parse the [GPS Exchange (GPX)](http://www.topografix.com/GPX/1/1/) and [OpenStreetMap (OSM)](http://wiki.openstreetmap.org/wiki/API_v0.6/DTD) data formats.

[Here](http://code.google.com/p/geo-gpx/) are some example uses of parsing GPX files and [here](http://code.google.com/p/geo-osm/) are examples parsing OSM files. My favourite is a very simple example (there are more complex ones) that removes waypoints from a GPX file. [This question (how to remove waypoints from gpx?)](http://lists.openstreetmap.org/pipermail/newbies/2009-May/003131.html) was asked on the OSM mailing list quite a while ago; questions like these partially inspired me to write these libraries.

~~~{.Haskell}
import Data.Geo.GPX

removeWpts :: FilePath -> FilePath -> IO ()
removeWpts = flip interactGpx (usingWpts (const []))
~~~

The implementation is very simple. The `interactGpx` function takes two file names and a function that transforms a `Gpx` data structure to a new `Gpx`. The `interactGpx` function reads in the first given file name to a `Gpx`, executes the given function to produce a new `Gpx`, then writes the result to the other given file name.

~~~{.Haskell}
interactGpx :: FilePath -> (Gpx -> Gpx) -> FilePath -> IO ()
~~~

The `usingWpts` function takes a function that transforms a list of waypoints to a new list of waypoints and a `Gpx` value and returns a new `Gpx` value with the waypoints transformed.

~~~{.Haskell}
usingWpts :: ([WptType] -> [WptType]) -> Gpx -> Gpx
~~~

Of course, since we want to remove all waypoints, we ignore the given list of waypoints and return an empty list (`const []`). Pretty neat I reckon!

You can get either of these libraries from [hackage](http://hackage.haskell.org/):



	
  * [GPX](http://hackage.haskell.org/package/GPX)

	
  * [OSM](http://hackage.haskell.org/package/OSM)


Here is each of their home page:

	
  * [GPX](http://code.google.com/p/geo-gpx/)

	
  * [OSM](http://code.google.com/p/geo-osm/)



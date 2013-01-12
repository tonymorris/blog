#!/bin/sh

rsync -aH --delete _site/ data:/var/www/blog.tmorris.net

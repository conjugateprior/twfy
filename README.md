# `twfy` 

[![Travis-CI Build Status](https://travis-ci.org/conjugateprior/twfy.svg?branch=master)](https://travis-ci.org/conjugateprior/twfy)

This package wraps TheyWorkForYou's API for R.
[TheyWorkForYou](http://www.theyworkforyou.com) is a parliamentary
monitoring site that scrapes and repackages Hansard (the UK's
parliamentary record) and augments it with information from the
Register of Members' Interests, election results, and voting records
to provide a unified source of information about UK legislators and
their activities.

## Set up

You'll need an API key to. Get it 
[http://www.theyworkforyou.com/api](here). 

Your first API call (or call to `set_api_key`) will prompt you to
paste it into the R console, after which it will be available for all
future R sessions.

## Results

While API call results typically returned as `json` in the background
they are parsed into nested `data.frame`s and `list`s using
`jsonlite`.  One exception is `getBoundary` which returns a string of
KML.

## Documentation

Package documentation is incomplete.  A more nearly complete set of
documentation can be found from the [API source](http://www.theyworkforyou.com/api).



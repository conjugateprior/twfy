# `twfy` 

[![Travis-CI Build Status](https://travis-ci.org/conjugateprior/twfy.svg?branch=master)](https://travis-ci.org/conjugateprior/twfy)

This package wraps TheyWorkForYou's API for R.

## Set up

You'll need an API key to. Get it 
[http://www.theyworkforyou.com/api](here). 

Your first API call (or call to 
`set_api_key`) will prompt you to paste it into the R console, after which it 
will be available for all future R sessions.

## Results

API call results typically returned as `json` in the background 
but parsed into nested `data.frame`s and `list`s using `jsonlite`.  
One exception is `getBoundary` which returns a string of KML.

## Documentation

Package documentation is incomplete.  A more nearly but still not complete set 
of documentation can be found at the link above.



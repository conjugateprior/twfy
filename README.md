# `twfy` 

[![Travis-CI Build Status](https://travis-ci.org/conjugateprior/twfy.svg?branch=master)](https://travis-ci.org/conjugateprior/twfy) [![CRAN_Status_Badge](http://www.r-pkg.org/badges/version/twfy)](https://cran.r-project.org/package=twfy)

This package wraps TheyWorkForYou's API for R.
[TheyWorkForYou](http://www.theyworkforyou.com) is a parliamentary
monitoring site that scrapes and repackages Hansard (the UK's
parliamentary record) and augments it with information from the
Register of Members' Interests, election results, and voting records
to provide a unified source of information about UK legislators and
their activities.

## Set up

You'll need an API key to. Get it [here](http://www.theyworkforyou.com/api). 

Your first API call (or call to `set_api_key`) will prompt you to
paste it into the R console, after which it will be available for all
future R sessions.

## Installation

```
install.packages("twfy")
```
or you can install the latest develeopment version from Github using `devtools`:
```
devtools::install_github("conjugateprior/twfy")
```

## Documentation

Package documentation is incomplete.  A more nearly complete set of
documentation can be found from the [API source](http://www.theyworkforyou.com/api).



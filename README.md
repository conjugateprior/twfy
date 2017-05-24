# `twfy`

## An R interface to the TheyWorkForYou API

You'll need an API key. Get it 
[http://www.theyworkforyou.com/api](here). Your first call will prompt you 
to paste it into the R console, after which it will be available as `Sys.getenv("TWFY_API_KEY")`

API call results are returned as `json` in the background but parsed into 
possibly nested `list`s and `data.frame`s using `jsonlite` for consumption.

Package documentation is incomplete.  A more nearly complete set of documentation
can be found at the link above.

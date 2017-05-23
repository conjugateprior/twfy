set_api_key <- function(key){
  twfy_api_key <<- key
  twfy_access_point <<- "https://www.theyworkforyou.com/api/"
  message("Api key set.")
}

call_api <- function(verb, ...){
  q <- list(...)
  q <- q[sapply(q, function(x) !is.null(x))] # remove NULL values

  q$output <- "js"
  q$key <- twfy_api_key
  resp <- httr::GET(paste0(twfy_access_point, verb), query=q)
  robj <- rjson::fromJSON(httr::content(resp))
  if ("error" %in% names(robj))
    stop(robj$error)
  robj
}

mkquery <- function(lst){
  lst$verb <- deparse(lst[[1]]) # add verb to list
  lst[2:length(lst)] # remove specious unnamed first element
}

## ------

convertURL <- function(url){
  params <- mkquery(as.list(match.call()))
  do.call("call_api", params)
}

## ------

getConstituency <- function(name=NULL, postcode=NULL){
  params <- mkquery(as.list(match.call()))
  do.call("call_api", params)
}

# list, or if simplify: char vector
getConstituencies <- function(simplify=TRUE){
  cons <- call_api("getConstituencies")
  if (simplify)
    return(unlist(lapply(cons, function(x) x$name)))
  cons
}

## ------

getGeometry <- function(name){
  params <- mkquery(as.list(match.call()))
  do.call("call_api", params)
}

getBoundary <- function(name){
  stop("Not implemented. Returns a KML file apparently")
}

## ------

getLords <- function(date=NULL, party=NULL, search=NULL){
  params <- mkquery(as.list(match.call()))
  do.call("call_api", params)
}

getLord <- function(id){
  params <- mkquery(as.list(match.call()))
  do.call("call_api", params)
}

getMPs <- function(date=NULL, party=NULL, search=NULL){
  params <- mkquery(as.list(match.call()))
  do.call("call_api", params)
}

getMP <- function(id=NULL, postcode=NULL, constituency=NULL,
                  always_return=NULL){
  params <- mkquery(as.list(match.call()))
  do.call("call_api", params)
}

# fields should be comma separated values
getMPInfo <- function(id, fields=NULL){
  params <- mkquery(as.list(match.call()))
  do.call("call_api", params)
}

getMPsInfo <- function(ids, fields=NULL){
  params <- mkquery(as.list(match.call()))
  do.call("call_api", params)
}

getPerson <- function(id){
  params <- mkquery(as.list(match.call()))
  do.call("call_api", params)
}

getMLA <- function(id=NULL,postcode=NULL, constituency=NULL,
                   always_return=NULL){
  params <- mkquery(as.list(match.call()))
  do.call("call_api", params)
}

getMLAs <- function(date=NULL, party=NULL, search=NULL){
  params <- mkquery(as.list(match.call()))
  do.call("call_api", params)
}

getMSP <- function(id=NULL, postcode=NULL, constituency=NULL,
                   always_return=NULL){
  params <- mkquery(as.list(match.call()))
  do.call("call_api", params)
}

getMSPs <- function(date=NULL, party=NULL, search=NULL){
  params <- mkquery(as.list(match.call()))
  do.call("call_api", params)
}

getCommittee <- function(name=NULL, date=NULL){
  params <- mkquery(as.list(match.call()))
  do.call("call_api", params)
}

## ------

getDebates <- function(type=c("commons", "westminsterhall", "lords",
                             "scotland", "northernireland"),
                      date=NULL, search=NULL, person=NULL,
                      gid=NULL, order=c("d", "r"), page=NULL, num=NULL){
  params <- mkquery(as.list(match.call()))
  params$type <- match.call(type)
  params$order <- match.call(order)
  do.call("call_api", params)
}

getWrans <- function(date=NULL, search=NULL, person=NULL,
                     gid=NULL, order=c("d", "r"), page=NULL, num=NULL){
  params <- mkquery(as.list(match.call()))
  params$order <- match.call(order)
  do.call("call_api", params)
}

getWMS <- function(date=NULL, search=NULL, person=NULL,
                     gid=NULL, order=c("d", "r"), page=NULL, num=NULL){
  params <- mkquery(as.list(match.call()))
  params$order <- match.call(order)
  do.call("call_api", params)
}

getHansard <- function(search=NULL, person=NULL, order=c("d", "r"),
                       page=NULL, num=NULL){
  params <- mkquery(as.list(match.call()))
  params$order <- match.call(order)
  do.call("call_api", params)
}

## ------

getComments <- function(start_date=NULL, end_date=NULL, search=NULL,
                        pid=NULL, page=NULL, num=NULL){
  params <- mkquery(as.list(match.call()))
  do.call("call_api", params)
}

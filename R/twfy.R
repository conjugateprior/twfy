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

# list, or if simplify: char vector
getConstituencies <- function(simplify=TRUE){
  cons <- call_api("getConstituencies")
  if (simplify)
    return(unlist(lapply(cons, function(x) x$name)))
  cons
}

construct_arg_list <- function(lst){
  lst$verb <- deparse(lst[[1]]) # add verb to list
  lst[2:length(lst)] # remove specious unnamed first element
}

# A rather good way to confuse everybody
getConstituency <- function(name=NULL, postcode=NULL){
  params <- as.list(match.call())
  do.call("call_api", construct_arg_list(params))
}

getGeometry <- function(name){
  params <- as.list(match.call())
  do.call("call_api", construct_arg_list(params))
}

getBoundary <- function(name){
  stop("Not implemented. Returns a KML file apparently")
}

getLords <- function(date=NULL, party=NULL, search=NULL){
  params <- as.list(match.call())
  do.call("call_api", construct_arg_list(params))
}

getLord <- function(id){
  params <- as.list(match.call())
  do.call("call_api", construct_arg_list(params))
}

getMPs <- function(date=NULL, party=NULL, search=NULL){
  params <- as.list(match.call())
  do.call("call_api", construct_arg_list(params))
}

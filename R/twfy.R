add_key_to_renviron <- function(key){
  dotRenviron <- file.path(normalizePath("~/"), ".Renviron")
  twfy_line <- paste0("TWFY_API_KEY=", key)
  if (file.exists(dotRenviron)){
    lines <- readLines(dotRenviron)
    tline <- which(grepl("TWFY_API_KEY", lines))
    if (length(tline) == 0) # no entry
      lines <- c(lines, twfy_line) # add as a final line
    else
      lines[tline] <-  twfy_line # overwrite existing entry
    writeLines(lines, dotRenviron)
  } else {
    writeLines(twfy_line, dotRenviron) # create file and write key to it
  }
}

get_api_key <- function(){
  key <- Sys.getenv("TWFY_API_KEY")
  if (key == ""){
    key <- readline(prompt="Paste in your API key (or just press return to stop) ")
    if (key != ""){
      Sys.setenv(TWFY_API_KEY=key)
      add_key_to_renviron(key)  # and set up for next time
    } else
      stop("Hint: you can request an API key from http://theyworkforyou.com/api/key")
  }
  key
}


#' Call the API directly
#'
#' All the other functions call this one. (It's exported only
#' for debugging purposes).  Use them instead.
#'
#' If you're really curious about implementation, read on.
#' Each API function introspects to see what its function name
#' is, bundles up its named arguments, and calls this function with them.
#' Consequently, aside from a bit of argument checking and/or return value
#' massaging, every function is implemented exactly the same way.
#'
#' @param endpoint function name e.g. 'getConstituencies'
#' @param ... often optional named arguments
#'
#' @return the response content, as unserialized by \code{jsonlite::fromJSON}
#' @export
call_api <- function(endpoint, ...){
  q <- list(...)
  if (length(q) > 0)
    q <- q[sapply(q, function(x) !is.null(x))] # remove NULL values

  q$output <- "js"
  q$key <- get_api_key()

  twfy_url <- "https://www.theyworkforyou.com/api/"
  resp <- httr::GET(paste0(twfy_url, endpoint), query=q)
  if (http_type(resp) != "application/json")
    stop("API did not return json", call. = FALSE) # did the API change?

  robj <- jsonlite::fromJSON(httr::content(resp))
  # API errors return 200 but provide a field in the json
  if ("error" %in% names(robj)) #
    stop(robj$error)
  robj
}

params_from_call <- function(mcall){
  lst <- as.list(mcall)
  lst$endpoint <- deparse(lst[[1]]) # add endpoint to list
  lst[2:length(lst)] # remove specious unnamed first element
}

#' Convert URL
#'
#' Converts a parliament.uk Hansard URL into a
#' TheyWorkForYou one, if possible.
#'
#' @param url url you want to convert
#'
#' @return A list containing \code{gid} and \code{url}.
#' @export
#'
convertURL <- function(url){
  params <- params_from_call(match.call())
  do.call("call_api", params)
}

## ------

#' Get information for a constituency
#'
#' @param name Name of constituency
#' @param postcode A postcode
#'
#' One of \code{name} or \code{postcode} is required.
#'
#' @return A data.frame with columns
#'        \itemize{
#'          \item{\code{name}}{Constituency name}
#'          \item{\code{pa_id}}{Constituency identifier}
#'          \item{\code{bbc_constituency_id}}{BBC numeric identifier}
#'          \item{\code{guardian_election_results}}{URL for election results}
#'          \item{\code{guardian_id}}{Guardian numeric identifier}
#'          \item{\code{guardian_name}}{Guardian name identifier}
#'        }
#' @export
getConstituency <- function(name=NULL, postcode=NULL){
  params <- params_from_call(match.call())
  res <- do.call("call_api", params)
  data.frame(res, stringsAsFactors = FALSE) # flatten to df
}

#' Get constituency names
#'
#' @param date Date for which constituency names are required
#' @param search Search string
#'
#' @return a data.frame with single column \code{name} containing
#'         constituency names
#' @export
getConstituencies <- function(date=NULL, search=NULL){
  params <- params_from_call(match.call())
  do.call("call_api", params)
}

## ------

#' Return geometry information for a constituency
#'
#' From TheyWorkForYou: "This currently includes, for Great Britain,
#' the latitude and longitude of the centre point of the bounding box
#' of the constituency, its area in square metres, the bounding box itself
#' and the number of parts in the polygon that makes up the constituency.
#' For Northern Ireland, as we don't have any better data, it only returns
#' an approximate (estimated by eye) latitude and longitude for the
#' constituency's centroid."
#'
#'
#' @param name Name of constituency
#'
#' @return A data.frame with columns \itemize{
#'  \item{\code{parts} }{number of connected parts of the constituency}
#'  \item{\code{area} }{area in square meters}
#'  \item{\code{min_lat} }{minimum latitude in the constituency}
#'  \item{\code{centre_lat} }{latitude of the central point of the constituency}
#'  \item{\code{max_lat} }{maximum latitude in the constituency}
#'  \item{\code{min_long} }{minimum longitude in the constituency}
#'  \item{\code{centre_long} }{longitude of the central point of the constituency}
#'  \item{\code{max_long} }{maximum longitude in the constituency}
#'  \item{\code{srid_n} }{}
#'  \item{\code{min_e} }{}
#'  \item{\code{centre_e} }{}
#'  \item{\code{max_e} }{}
#'  \item{\code{min_n} }{}
#'  \item{\code{centre_n} }{}
#'  \item{\code{max_n} }{}
#' }
#' @export
getGeometry <- function(name){
  params <- params_from_call(match.call())
  res <- do.call("call_api", params)
  data.frame(res, stringsAsFactors = FALSE) # flatten to df
}

#' Get constituency boundary as a KML file
#'
#' NOTE: This is not implemented.
#'
#' @param name Name of constituency
#'
#' @return KML file
#' @export
getBoundary <- function(name){
  stop("Not yet implemented.")
}

## ------


#' Get information about Lords
#'
#' @param date ISO-style date, e.g. "1990-01-02", to compile a list of Lords for
#' @param party Restrict list to Lords in this party
#' @param search A search term
#'
#' @return A data.frame with columns
#' \itemize{
#'   \item{\code{member_id} }{Member identifier, dependent on position}
#'   \item{\code{person_id} }{Person identifier}
#'   \item{\code{name} }{Name of Lord}
#'   \item{\code{party} }{Party}
#'   \item{\code{office} }{NULL if no offices held, or a list with one
#'     element: a data.frame with column headings:
#'     \itemize{
#'       \item{\code{dept} }{The committee or commission name}
#'       \item{\code{position} }{Job role, e.g. Member or Leader},
#'       \item{\code{from_date} }{Date tenure in office began}
#'       \item{\code{to_date} }{Date tenure in office ended, or
#'                             \code{9999-12-31} if still in office}
#'     }
#'   }
#' }
#' @export
getLords <- function(date=NULL, party=NULL, search=NULL){
  params <- params_from_call(match.call())
  do.call("call_api", params)
}


#' Get information about a Lord
#'
#' @param id Person identifier
#'
#' @return a data.frame with columns
#' \itemize{
#'   \item{\code{member_id} }{Member identifier}
#'   \item{\code{house} }{Which chamber 1: The House of Commons,
#'                       2: The House of Lords.}
#'   \item{\code{constituency} }{Consituency represented}
#'   \item{\code{party} }{Party}
#'   \item{\code{entered_house} }{Date when entered the House of Lords}
#'   \item{\code{left_house} }{Date when left the House of Lords or
#'                              \code{9999-12-31} if still in office}
#'   \item{\code{entered_reason} }{Why they entered}
#'   \item{\code{left_reason} }{Why they left}
#'   \item{\code{person_id} }{Person identifier}
#'   \item{\code{lastupdate} }{Date TheyWorkForYou updated this information}
#'   \item{\code{title} }{Title, e.g. 'Baroness'}
#'   \item{\code{given_name} }{First names}
#'   \item{\code{family_name} }{Family name}
#'   \item{\code{lordofname} }{Location associated with title}
#'   \item{\code{full_name} }{First names then family name}
#' }
#' @export
getLord <- function(id){
  params <- params_from_call(match.call())
  do.call("call_api", params)
}

#' Get information about Members of Parliament
#'
#' @param date Date for which the MP list is constructed
#' @param party Restrict to MPs in this party
#' @param search A search string
#'
#' @return a data.frame with columns \itemize{
#'  \item{\code{member_id} }{Member identifier}
#'  \item{\code{person_id} }{Person identifier}
#'  \item{\code{name} }{MP's first names then family name}
#'  \item{\code{party} }{Party}
#'  \item{\code{constituency} }{Consituency represented}
#'  \item{\code{office} }{NULL if no offices held, or a list with one
#'     element: a data.frame with column headings:
#'     \itemize{
#'       \item{\code{dept} }{The committee or commission name}
#'       \item{\code{position} }{Role, e.g. Member},
#'       \item{\code{from_date} }{Date tenure in office began}
#'       \item{\code{to_date} }{Date tenure in office ended, or
#'                             \code{9999-12-31} if still in office}
#'     }
#'   }
#' } or an empty list if parliament is dissolved.
#' @export
getMPs <- function(date=NULL, party=NULL, search=NULL){
  params <- params_from_call(match.call())
  mps <- do.call("call_api", params)
  if (length(mps) == 0) # Catch a basic naive error around election time
    stop("No results. (Note that there are no MPs when Parliament is dissolved near to elections. If this is the reason, specifying a date may help)")
  mps
}

#' Get information about a Member of Parliament
#'
#' Returns
#' @param id An MP identifier
#' @param postcode A postcode, used to identify a constituency and thereby an MP
#' @param constituency Name of a constituency
#' @param always_return whether to try to return an MP even if
#'                      the seat is vacant or it before an election
#'
#' @return a data.frame with rows representing the MP's spells in Parliament
#' and columns \itemize{
#'   \item{\code{member_id} }{Member identifier for each spell in parliament}
#'   \item{\code{house} }{1: House of Commons, 2: House of Lords}
#'   \item{\code{constituency} }{Constituency represented}
#'   \item{\code{party} }{Party}
#'   \item{\code{entered_house} }{Date MP entered Parliament}
#'   \item{\code{left_house} }{Date MP left Parliament}
#'   \item{\code{entered_reason} }{Reason MP entered, e.g. general_election}
#'   \item{\code{left_reason} }{Reason MP left, e.g. general_election_standing}
#'   \item{\code{person_id} }{Person identifier}
#'   \item{\code{lastupdate} }{When TheyWorkForYou last updated this information}
#'   \item{\code{title} }{Title, if any}
#'   \item{\code{given_name} }{First name}
#'   \item{\code{family_name} }{Family name}
#'   \item{\code{full_name} }{First name and family name}
#'   \item{\code{url} }{URL path relative to TheyWorkForYou's hostname}
#'   \item{\code{image} }{URL path to jpg relative to TheyWorkForYou's hostname}
#'   \item{\code{image_height} }{Image height in pixels}
#'   \item{\code{image_width} }{Image width in pixels}
#' }
#'
#' @export
getMP <- function(id=NULL, postcode=NULL, constituency=NULL,
                  always_return=NULL){
  params <- params_from_call(match.call())
  do.call("call_api", params)
}

#' Fetch more information about a Member of Parliament
#'
#' Unless you want all the four hundred or so fields available about an MP you should
#' probably specify the ones you want in a comma-separated string to \code{fields}.
#'
#' Note that unlike other functions in this package the result is a list
#'
#'
#' @param id An MP identifier
#' @param fields A comma separated character vector of field names
#'
#' @return a list
#' @export
getMPInfo <- function(id, fields=NULL){
  params <- params_from_call(match.call())
  do.call("call_api", params)
}

#' Fetch more information about a Member of Parliament
#'
#' @param ids A character vector of comma-separated MP identifiers
#' @param fields A comma separated character vector of field names
#'
#' @return A list of list of MP information
#' @export
getMPsInfo <- function(ids, fields=NULL){
  params <- params_from_call(match.call())
  do.call("call_api", params)
}

#' Fetch information about a person
#'
#' @param id A person identifier
#'
#' @return A list of information about a person
#' @export
getPerson <- function(id){
  params <- params_from_call(match.call())
  do.call("call_api", params)
}


#' Fetch a member of the London Assembly
#'
#' @param id A MLA identifier
#' @param postcode A postcode, which specifies a region represented by the MLA
#' @param constituency Name of a constituency
#' @param always_return whether to try to return an MP even if
#'                      the seat is vacant or it before an election
#'
#' @return A list of information about an MLA
#' @export
getMLA <- function(id=NULL,postcode=NULL, constituency=NULL,
                   always_return=NULL){
  params <- params_from_call(match.call())
  do.call("call_api", params)
}

#' Fetch information about MLAs
#'
#' @param date The date for which the MLAs are required
#' @param party Restrict MLAs to those in a party
#' @param search A search string
#'
#' @return A list of lists of MLA information
#' @export
getMLAs <- function(date=NULL, party=NULL, search=NULL){
  params <- params_from_call(match.call())
  do.call("call_api", params)
}

#' Fetch information on a Member of the Scottish Parliament
#'
#' @param id An MSP identifier
#' @param postcode A postcode, which specifies the constiuency whose MSP is required
#' @param constituency Name of a constituency
#' @param always_return whether to try to return an MP even if
#'                      the seat is vacant or it before an election
#'
#' @return A list of information about an MSP
#' @export
getMSP <- function(id=NULL, postcode=NULL, constituency=NULL,
                   always_return=NULL){
  params <- params_from_call(match.call())
  do.call("call_api", params)
}

#' Fetch information about Members of the Scottish Parliament
#'
#' @param date The date for which the MSPs are required
#' @param party Restrict MSPs to those in a party
#' @param search A search string
#'
#' @return A list of lists of SP information
#' @export
getMSPs <- function(date=NULL, party=NULL, search=NULL){
  params <- params_from_call(match.call())
  do.call("call_api", params)
}

#' Fetch members of a parliamentary select committee
#'
#' If \code{name} is not specified, all committees and their members are
#' returned.
#'
#' TheyWorkForYou notes that "We have no information since the
#' 2010 general election, and information before may be inaccurate."
#'
#' @param name Name of the committee. Partial names will be matched.
#' @param date Date for which the committee membership is required
#'
#' @return A list of committee members, and also committees if \code{name} is
#'         not specified.
#' @export
getCommittee <- function(name=NULL, date=NULL){
  params <- params_from_call(match.call())
  do.call("call_api", params)
}

## ------


#' Get information about parliamentary debates
#'
#' @param type location of the debate. Defaults to the House of Commons
#' @param date Date on which debates are required
#' @param search A search string
#' @param person A person identifier to specify which speaker's contributions are required
#' @param gid A speech/debate identifier to restrict to a particular debate
#' @param order whether to order results by date or relevance. Defaults to date
#' @param page which page of results to provide. Defaults to first page
#' @param num Number of results to return
#'
#' @return A complex list of debate information
#' @export
getDebates <- function(type=c("commons", "westminsterhall", "lords",
                             "scotland", "northernireland"),
                      date=NULL, search=NULL, person=NULL,
                      gid=NULL, order=c("d", "r"), page=NULL, num=NULL){
  params <- params_from_call(match.call())
  params$type <- match.arg(type)
  params$order <- match.arg(order)
  do.call("call_api", params)
}

#' Get written answers to questions
#'
#' @param date Date for which answers are required
#' @param search A search string
#' @param person A person identifier to specify who provided the answers
#' @param gid A written question and answer identifier to return
#' @param order whether to order results by date or relevance. Defaults to date
#' @param page which page of results to provide. Defaults to first page
#' @param num Number of results to return
#'
#' @return A list of written answers information
#' @export
getWrans <- function(date=NULL, search=NULL, person=NULL,
                     gid=NULL, order=c("d", "r"), page=NULL, num=NULL){
  params <- params_from_call(match.call())
  params$order <- match.arg(order)
  do.call("call_api", params)
}

#' Fetch written ministerial responses
#'
#' @param date Date for which responses are required
#' @param search A search string
#' @param person A person identifier to specify which minister provided the answers
#' @param gid A reponse identifier to return
#' @param order whether to order results by date or relevance. Defaults to date
#' @param page which page of results to provide. Defaults to first page
#' @param num Number of results to return
#'
#' @return A list of information about written ministerial responses
#' @export
getWMS <- function(date=NULL, search=NULL, person=NULL,
                     gid=NULL, order=c("d", "r"), page=NULL, num=NULL){
  params <- params_from_call(match.call())
  params$order <- match.arg(order)
  do.call("call_api", params)
}

#' Search Hansard
#'
#' @param search A search string
#' @param person A person identifier
#' @param order whether to order results by date or relevance. Defaults to date
#' @param page which page of results to provide. Defaults to first page
#' @param num Number of results to return
#'
#' @return A list of search results
#' @export
getHansard <- function(search=NULL, person=NULL, order=c("d", "r"),
                       page=NULL, num=NULL){
  params <- params_from_call(match.call())
  params$order <- match.arg(order)
  do.call("call_api", params)
}

## ------


#' Fetch comments left on TheyWorkForYou
#'
#' @param start_date Beginning date
#' @param end_date End date
#' @param search A search string
#' @param pid A person identifier
#' @param page which page of results to provide. Defaults to first page
#' @param num Number of results to return
#'
#' @return A list of user comments
#' @export
getComments <- function(start_date=NULL, end_date=NULL, search=NULL,
                        pid=NULL, page=NULL, num=NULL){
  params <- params_from_call(match.call())
  do.call("call_api", params)
}
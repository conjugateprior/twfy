ask_for_key <- function(){
  readline(prompt="Paste in your API key (or just press return to stop) ")
}

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

#' Set the API key
#'
#' This function manually sets or resets the API key.  It is mostly useful for
#' updating your key as all API function calls will ask for a key and store
#' it locally and in \code{~/.Renviron} any time they do not find one.
#'
#' @param api_key a new API key
#'
#' @return Nothing
#' @export
set_api_key <- function(api_key){
  Sys.setenv(TWFY_API_KEY=api_key)
  add_key_to_renviron(api_key)
}

#' Get the API key
#'
#' Get the current API key or request it if not present.  When you provide a key
#' it will be stored as a local environment variable \code{TWFY_API_KEY} and also
#' in your \code{~/.Renviron}, making it available for all subsequent R sessions.
#'
#' API keys can be requested at \url{http://theyworkforyou.com/api/key}.
#'
#' @return The current API key
#' @export
get_api_key <- function(){
  key <- Sys.getenv("TWFY_API_KEY")
  if (key == ""){
    key <- ask_for_key()
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
#' All the other functions call this one - it's exported only
#' for debugging purposes - so use them instead.
#'
#' If you're really curious about implementation, read on.
#' Each API function introspects to see what its function name
#' is, bundles up its named arguments, and calls this function with them.
#' Consequently, aside from a bit of argument checking and/or return value
#' massaging, every function is implemented exactly the same way.
#'
#' @param endpoint Function name e.g. 'getConstituencies'
#' @param ... \code{endpoint}'s (often optional) named arguments
#'
#' @return Response content, as unserialized by \code{jsonlite::fromJSON}
#' @export
call_api <- function(endpoint, ...){
  q <- list(...)
  if (length(q) > 0)
    q <- q[sapply(q, function(x) !is.null(x))] # remove NULL values

  q$output <- "js"
  q$key <- get_api_key()

  twfy_url <- "https://www.theyworkforyou.com/api/"
  resp <- httr::GET(paste0(twfy_url, endpoint), query=q)

  # if (httr::http_type(resp) != "application/json")
  #   stop("API did not return json", call. = FALSE) # API change?
  # The server actually offers text/javascript; ; charset=iso-8859-1 :-/

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
#' @return A one row data.frame with columns \code{gid} and \code{url}.
#' @export
#'
#' @examples
#' \dontrun{
#'
#' site <- "http://www.publications.parliament.uk"
#' page <- "/pa/cm201212/cmhansrd/cm120418/debtext/120418-0001.htm"
#' in_page <- "#12041847002086"
#' address <- paste0(site, page, in_page)
#'
#' res <- convertURL(address)
#' names(res) # gid, url
#' }

convertURL <- function(url){
  params <- params_from_call(match.call())
  res  <- do.call("call_api", params)
  data.frame(res, stringsAsFactors = FALSE) # for consistency with the rest
}

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

#' Get constituency boundary information as KML
#'
#' @param name Name of constituency
#'
#' @return A character vector full of KML (an XML dialect)
#'
#' @export
getBoundary <- function(name){
  # this is special case because it does not return JSON
  twfy_url <- "https://www.theyworkforyou.com/api/"
  resp <- httr::GET(paste0(twfy_url, "getBoundary"),
                    query=list(key=get_api_key(), name=name))
  httr::content(resp, as="text")
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
#'   \item{\code{constituency} }{Constituency represented}
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
#'  \item{\code{constituency} }{Constituency represented}
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
#'   \item{\code{house} }{1: House of Commons, 2: House of Lords, 3: Legislative Assembly (NI), 4: Scottish Parliament}
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
  res <- do.call("call_api", params)
  data.frame(res, stringsAsFactors = FALSE)
}

#' Get more information about a Member of Parliament
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

#' Get more information about a Member of Parliament
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

#' Get information about a person
#'
#' @param id A person identifier
#'
#' @return a data.frame with rows representing the person's spells in the whatever
#'         legislative body they are members of, with columns \itemize{
#'   \item{\code{member_id} }{Member identifier for each spell in the Assembly}
#'   \item{\code{house} }{1: House of Commons, 2: House of Lords, 3: Legislative Assembly (NI), 4: Scottish Parliament}
#'   \item{\code{constituency} }{Constituency represented}
#'   \item{\code{party} }{Party (in that spell)}
#'   \item{\code{entered_house} }{Date MP entered Parliament}
#'   \item{\code{left_house} }{Date MP left Parliament, or 9999-12-31 if still in place}
#'   \item{\code{entered_reason} }{Reason MP entered, e.g. general_election}
#'   \item{\code{left_reason} }{Reason MP left, e.g. general_election_standing}
#'   \item{\code{person_id} }{Person identifier}
#'   \item{\code{lastupdate} }{When TheyWorkForYou last updated this information}
#'   \item{\code{title} }{Title, if any}
#'   \item{\code{given_name} }{First name}
#'   \item{\code{family_name} }{Family name}
#'   \item{\code{full_name} }{First name and family name}
#' }
#' @export
getPerson <- function(id){
  params <- params_from_call(match.call())
  do.call("call_api", params)
}


#' Get information on a Member of the Legislative Assembly (Northern Ireland)
#'
#' @param id A MLA identifier
#' @param postcode A postcode, which specifies a region represented by the MLA
#' @param constituency Name of a constituency
#' @param always_return whether to try to return an MP even if
#'                      the seat is vacant or it before an election
#'
#' @return a data.frame with rows representing the MLA's spells in the Assembly
#' and columns \itemize{
#'   \item{\code{member_id} }{Member identifier for each spell in the Assembly}
#'   \item{\code{house} }{1: House of Commons, 2: House of Lords, 3: Legislative Assembly (NI), 4: Scottish Parliament}
#'   \item{\code{constituency} }{Constituency represented}
#'   \item{\code{party} }{Party}
#'   \item{\code{entered_house} }{Date MP entered Parliament}
#'   \item{\code{left_house} }{Date MP left Parliament, or 9999-12-31 if still in place}
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
#' @export
getMLA <- function(id=NULL,postcode=NULL, constituency=NULL,
                   always_return=NULL){
  params <- params_from_call(match.call())
  do.call("call_api", params)
}

#' Get information on Members of the Legislative Assembly (Northern Ireland)
#'
#' @param date The date for which the MLAs are required
#' @param party Restrict MLAs to those in a party
#' @param search A search string
#'
#' @return A data.frame with columns \itemize{
#'   \item{\code{member_id} }{Member identifier for each spell in the Legislative Assembly}
#'   \item{\code{person_id} }{Person identifier}
#'   \item{\code{name} }{Full name of MLA}
#'   \item{\code{party} }{Party represented}
#'   \item{\code{constituency} }{Name of MLA's constituency}
#' }
#
#' @export
getMLAs <- function(date=NULL, party=NULL, search=NULL){
  params <- params_from_call(match.call())
  do.call("call_api", params)
}

#' Get information on a Member of the Scottish Parliament
#'
#' @param id Person identifier
#' @param postcode A postcode, which specifies the constiuency whose MSP is required
#' @param constituency Name of a constituency
#' @param always_return whether to try to return an MP even if
#'                      the seat is vacant or it before an election
#'
#' @return a data.frame with rows representing the MP's spells in Parliament
#' and columns \itemize{
#'   \item{\code{member_id} }{Member identifier for each spell in parliament}
#'   \item{\code{house} }{1: House of Commons, 2: House of Lords, 3: Legislative Assembly (NI), 4: Scottish Parliament}
#'   \item{\code{constituency} }{Constituency represented}
#'   \item{\code{party} }{Party}
#'   \item{\code{entered_house} }{Date MP entered Parliament}
#'   \item{\code{left_house} }{Date MP left Parliament, or 9999-12-31 if still in place}
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
#' @export
getMSP <- function(id=NULL, postcode=NULL, constituency=NULL,
                   always_return=NULL){
  params <- params_from_call(match.call())
  do.call("call_api", params)
}

#' Get information about Members of the Scottish Parliament
#'
#' @param date The date for which the MSPs are required
#' @param party Restrict MSPs to those in a party
#' @param search A search string
#'
#' @return A data.frame with columns \itemize{
#'   \item{\code{member_id} }{Member identifier for each spell in the Scottish parliament}
#'   \item{\code{person_id} }{Person identifier}
#'   \item{\code{name} }{Full name of MSP}
#'   \item{\code{party} }{Party represented}
#'   \item{\code{constituency} }{Name of MSP's constituency}
#' }
#'
#' @export
getMSPs <- function(date=NULL, party=NULL, search=NULL){
  params <- params_from_call(match.call())
  do.call("call_api", params)
}

#' Get members of a Parliamentary Select Committee
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
#' @return A data.frame of committee members, and also committees if \code{name} is
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
#' @return A complex data.frame of debate information.  Documentation is somewhat
#'         lacking, and many columns contain data.frames so you'll have to dig
#'         around.
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
#' The output of this function needs documentation.
#'
#' @param date Date for which answers are required
#' @param search A search string
#' @param person A person identifier to specify who provided the answers
#' @param gid A written question and answer identifier to return
#' @param order whether to order results by date or relevance. Defaults to date
#' @param page which page of results to provide. Defaults to first page
#' @param num Number of results to return
#'
#' @return A data.frame with two columns \itemize{
#'   \item{\code{entry} }{a data.frame}
#'   \item{\code{subs} }{a data.frame}
#' }.  The \code{entry} data.frame has columns \itemize{
#'   \item{\code{epobject_id}}{}
#'   \item{\code{htype}}{}
#'   \item{\code{gid}}{}
#'   \item{\code{hpos}}{}
#'   \item{\code{section_id}}{}
#'   \item{\code{subsection_id}}{}
#'   \item{\code{hdate}}{}
#'   \item{\code{htime}}{}
#'   \item{\code{source_url}}{}
#'   \item{\code{major}}{}
#'   \item{\code{minor}}{}
#'   \item{\code{video_status}}{}
#'   \item{\code{colnum}}{}
#'   \item{\code{body}}{}
#' }. \code{subs} is a list containing data.frames with all the columns in
#'  \code{entry} and in addition \itemize{
#'    \item{\code{excerpt}}{}
#'    \item{\code{listurl}}{}
#'    \item{\code{commentsurl}}{}
#'    \item{\code{totalcomments}}{}
#'    \item{\code{comment}}{}
#'  }
#' @export
getWrans <- function(date=NULL, search=NULL, person=NULL,
                     gid=NULL, order=c("d", "r"), page=NULL, num=NULL){
  params <- params_from_call(match.call())
  params$order <- match.arg(order)
  do.call("call_api", params)
}

#' Get written ministerial responses
#'
#' @param date Date for which responses are required
#' @param search A search string
#' @param person A person identifier to specify which minister provided the answers
#' @param gid A response identifier to return
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
#' This needs much more documentation.
#'
#' @param search A search string
#' @param person A person identifier
#' @param order whether to order results by date or relevance. Defaults to date
#' @param page which page of results to provide. Defaults to first page
#' @param num Number of results to return
#'
#' @return Search results
#' @export
getHansard <- function(search=NULL, person=NULL, order=c("d", "r"),
                       page=NULL, num=NULL){
  params <- params_from_call(match.call())
  params$order <- match.arg(order)
  do.call("call_api", params)
}

## ------


#' Get comments left on TheyWorkForYou
#'
#' @param start_date Beginning date
#' @param end_date End date
#' @param search A search string
#' @param pid A person identifier
#' @param page which page of results to provide. Defaults to first page
#' @param num Number of results to return
#'
#' @return A list containing data.frames with columns \itemize{
#'   \item{\code{comment_id}}{Comment identifier}
#'   \item{\code{user_id}}{User identifier}
#'   \item{\code{epobject_id}}{}
#'   \item{\code{body}}{Text of comment}
#'   \item{\code{posted}}{date and time posted}
#'   \item{\code{major}}{}
#'   \item{\code{gid}}{}
#'   \item{\code{firstname}}{Commenter's first name}
#'   \item{\code{lastname}}{Commenter's last name}
#'   \item{\code{url}}{URL of the comment}
#'   \item{\code{useurl}}{URL endpoint for commenter}
#' }
#' @export
getComments <- function(start_date=NULL, end_date=NULL, search=NULL,
                        pid=NULL, page=NULL, num=NULL){
  params <- params_from_call(match.call())
  do.call("call_api", params)
}

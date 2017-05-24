#' twfy: A package for accessing TheyWorkForYou
#'
#' This package is a light wrapper around TheyWorkForYou's API,
#' allowing queries using the verbs described at
#' \url{http://theyworkforyou.com/api/}.  This shoudl be your primary
#' source of documentation as the package develops.
#'
#' Use of the API requires a key which can be acquired at the address above.
#' The first time you use a package function you will be prompted for your
#' API key.  This will then be stored as an environment variable in
#' \code{~/.Renviron}, creating that file if necessary, and you will not be
#' asked again.
#'
#' Currently there is no means of overriding a key with another key except
#' by editing \code{~/.Renviron}.
#'
#' @docType package
#' @name twfy
NULL

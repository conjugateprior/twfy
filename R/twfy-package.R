#' twfy: A package for accessing TheyWorkForYou
#'
#' This package is a light wrapper around TheyWorkForYou's API,
#' allowing queries using the verbs described at
#' \url{http://theyworkforyou.com/api/}.  This should be your primary
#' source of documentation as the package develops.
#'
#' Use of the API requires a key which can be acquired at the address above.
#' The first time you use a package function you will be prompted for your
#' API key.  This will then be stored as an environment variable and you will
#' not be asked again.
#'
#' To temporarily override the key being used, change the environment variable
#' \code{TWFY_API_KEY} with \code{Sys.setenv}.  To make the change permanent,
#' edit the relevant line in \code{~/.Renviron}.
#'
#' @docType package
#' @name twfy
NULL

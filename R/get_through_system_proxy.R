#' Uploads a document through the system's transparent proxy
#' A function to retrieve the proxy settings from the system and pass them as parameters to the GET function of the httr package.
#'
#' @param target_url a character string
#'
#' @return a list containing the results of the HTTP/HTTPS request
#' @import magrittr
#' @importFrom httr parse_url
#' @importFrom httr modify_url
#' @importFrom httr use_proxy
#' @importFrom httr GET
#' @importFrom curl ie_get_proxy_for_url
get_through_system_proxy <- function(target_url) {

  # Checks that the string is not empty or unique
  stopifnot(
    !is.null(target_url),
    !is.na(target_url),
    is.character(target_url),
    length(target_url) == 1,
    nchar(target_url) > 0
  )

  # Adds the protocol if it is not specified.
  if( is.null(parse_url(target_url)) ) {
    target_url %<>% modify_url(scheme = "http")
  }

  # Retrieves the configuration of the system proxy
  config_proxy <- ie_get_proxy_for_url(target_url)

  # Download the target document
  GET(target_url, use_proxy(config_proxy))

}

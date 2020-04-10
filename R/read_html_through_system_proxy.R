#' Parse an HTML document through the system's transparent proxy
#' A function downloading a document through the transparent proxy of the system then parsing it using function read_html of the xml2 package
#'
#' @param target_url a character string
#'
#' @return a xml_document object
#' @importFrom httr status_code
#' @importFrom httr content
#' @importFrom xml2 read_html
read_html_through_system_proxy <- function(target_url) {

  # Call function get_through_system_proxy to check the parameters and retrieve the target document.
  target_response <- get_through_system_proxy(target_url = target_url)

  # Checks that the query has been successful
  stopifnot( status_code(target_response) == 200 )

  # Checks that the content of the target document is not empty
  target_content <- content(target_response, encoding = "UTF-8", as = "text")
  stopifnot( nchar(target_content) > 0 )

  # Parse the html code of the document
  read_html(target_content)

}

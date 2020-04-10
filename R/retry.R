#' Retry a function in cas of failure
#'A function to restart a function in case of failure (source StackOverflow)
#'
#' @importFrom utils capture.output
#' @importFrom utils str
#'
#' @param expr expression to be evaluated
#' @param isError error control function
#' @param maxErrors Maximum number of possible failures
#' @param sleep time in seconds between two attempts
#'
#' @return the results of expr
retry <-
  function(expr,
           isError = function(x)
             "try-error" %in% class(x),
           maxErrors = 5,
           sleep = 0) {
    attemps = 0
    retval = try(eval(expr))
    while (isError(retval)) {
      attemps = attemps + 1
      if (attemps >= maxErrors) {
        msg = sprintf("retry: too many retries [[%s]]", capture.output(str(retval)))
        stop(msg)
      } else {
        msg = sprintf("retry: error in attemp %i/%i [[%s]]",
                      attemps,
                      maxErrors,
                      capture.output(str(retval)))
      }
      if (sleep > 0)
        Sys.sleep(sleep)
      retval = try(eval(expr))
    }
    return(retval)
  }

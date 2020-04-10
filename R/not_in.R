#' Not in
#' Checks that value of x is not in the table vector.
#'
#' @param x vector or NULL: the values not to be matched. Long vectors are supported.
#' @param table vector or NULL: the values to be matched against. Long vectors are not supported.
#'
#' @return a logical vector
"%not_in%" <- function(x, table) {
  result <- match(x, table, nomatch = 0)
  ifelse(result == 0, TRUE, FALSE)
}

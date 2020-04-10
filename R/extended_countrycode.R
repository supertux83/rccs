#' Extension of the countrycode function
#' A function to manage a wider range of iso3c codes and country names in French
#'
#' @import dplyr
#' @import magrittr
#' @import countrycode
#' @importFrom tibble tibble
#' @importFrom utils data
#' @importFrom rlang .data
#'
#' @param x Vector which contains the codes or country names to be converted
#' @param origin Coding scheme of origin (string such as "iso3c" enclosed in quotes "")
#' @param destination Coding scheme of destination (string such as "iso3c" enclosed in quotes "")
#'
#' @return x values in the destination coding scheme
#' @export
extended_countrycode <- function(x, origin, destination) {

  # Converts factors as character vector
  if( is.factor(x) ) {
    x <- as.character(x)
  }

  # Removal of useless spaces
  x <- trimws(x)

  # correspondence between the iso3c code and the english name
  if( origin == "iso3c" && destination %in% c("country.name", "country.name.en") ) {
    result <- iso3c %>%
      filter(.data$iso3c == x) %>%
      pull(.data$country_name_en)
    return(result)
  }

  # correspondence between the iso3c code and the french name
  if( origin == "iso3c" && destination == "country.name.fr" ) {
    result <- iso3c %>%
      filter(.data$iso3c == x) %>%
      pull(.data$country_name_fr)
    return(result)
  }

  # correspondence between the english name and the iso3c code
  if( origin %in% c("country.name", "country.name.en") && destination == "iso3c" ) {

    # Use only lower case
    x <- tolower(x)

    # Transformation of certain names known to be problematic
    x[x == "f.s. micronesia"] <- "Micronesia (Federated States of)"
    x[x == "aland islands"]   <- "\u00c5land Islands"
    x[x == "saint martin"]    <- "Saint Martin (FR)"
    x[x == "saint-martin"]    <- "Saint Martin (FR)"
    x[x == "st martin"]       <- "Saint Martin (FR)"
    x[x == "inner mongolia"]  <- "Mongolian part of China"

    # Identify as many codes as possible using the regular expressions of the native package
    result <- suppressWarnings({
      countrycode(x, origin = "country.name", destination = "iso3c")
    })

    # For missing values, we look to see if there is an exact match in the additional data
    df <- tibble(
      country_name_en = x,
      local_iso3c = result
    )
    tolower_iso3c <- iso3c %>%
      select(.data$country_name_en, .data$iso3c) %>%
      mutate(country_name_en = tolower(.data$country_name_en))
    df %<>% left_join(tolower_iso3c, by = "country_name_en")
    df %<>% mutate(final_iso3c = ifelse(is.na(.data$local_iso3c), .data$iso3c, .data$local_iso3c))

    # Returns codes that may have been identified
    final_result <- df %>% pull(.data$final_iso3c)
    return(final_result)

  }

  # If no conditions have been met, use the basic function
  countrycode(x, origin = origin, destination = destination)

}

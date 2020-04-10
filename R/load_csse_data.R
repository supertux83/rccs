#' Function for loading CSSE COVID-19 data from github
#'
#' @import dplyr
#' @import magrittr
#' @importFrom tidyselect all_of
#' @importFrom tibble tibble
#' @importFrom rlang .data
#' @importFrom httr status_code
#' @importFrom httr content
#' @importFrom readr read_csv
#' @importFrom lubridate mdy
#' @importFrom jsonlite fromJSON
#' @importFrom tidyr spread
#'
#' @param case_type a character string. Possible values : "confirmed", "deaths", "recovered"
#' @param sum_by_iso3c a logical variable, indicating whether cases should be aggregated by iso3c code
#' @param use_french_gouv_data a logical value
#'
#' @return a dataframe with the last csse github data
#' @export
load_csse_data <- function(case_type = "confirmed", sum_by_iso3c = TRUE, use_french_gouv_data = TRUE) {

  # Checking the case_type parameter
  stopifnot(
    length(case_type) == 1,
    case_type %in% c("confirmed", "deaths", "recovered")
  )
  stopifnot(
    length(sum_by_iso3c) == 1,
    is.logical(sum_by_iso3c)
  )
  stopifnot(
    length(use_french_gouv_data) == 1,
    is.logical(use_french_gouv_data)
  )

  # Download URLs
  csse_url <- list(
    "confirmed" = "https://raw.githubusercontent.com/CSSEGISandData/COVID-19/master/csse_covid_19_data/csse_covid_19_time_series/time_series_covid19_confirmed_global.csv",
    "deaths" = "https://raw.githubusercontent.com/CSSEGISandData/COVID-19/master/csse_covid_19_data/csse_covid_19_time_series/time_series_covid19_deaths_global.csv",
    "recovered" = "https://raw.githubusercontent.com/CSSEGISandData/COVID-19/master/csse_covid_19_data/csse_covid_19_time_series/time_series_covid19_recovered_global.csv"
  )

  # Download the raw data
  csse_raw_data <- get_through_system_proxy( csse_url[[case_type]] )
  stopifnot( status_code(csse_raw_data) == 200 )

  # CSV file retrieval
  csse_csv_data <- content(csse_raw_data, as = "text", encoding = "UTF-8")
  csse_df <- read_csv(csse_csv_data)

  # Retrieving an iso3c code for provinces and territories where possible
  province_iso3c <- sapply(csse_df[["Province/State"]], extended_countrycode, origin = "country.name", destination = "iso3c")

  # Retrieving the iso3c code of countries
  country_iso3c <- sapply(csse_df[["Country/Region"]], extended_countrycode, origin = "country.name", destination = "iso3c")

  # Merging iso3c code lists
  # If a code has not been identified, it may be necessary to update the program : returns a warning
  final_iso3c <- ifelse(is.na(province_iso3c), country_iso3c, province_iso3c)
  if( sum(is.na(final_iso3c)) > 0 ) {
    error_msgs <- csse_df %>%
      filter(is.na(final_iso3c)) %>%
      mutate(label = sprintf("ISO3C code not found for line : Province/state = %s, Country/region = %s", .data$`Province/State`, .data$`Country/Region`)) %>%
      pull(.data$label)
    sapply(error_msgs, warning)
  }
  final_iso3c[is.na(final_iso3c)] <- "XXX"

  # Retrieving columns with a date
  csse_colnames <- names(csse_df)
  indices_date_cols <- grep("[0-9]+/[0-9]+/[0-9]+", csse_colnames)
  csse_data_dates <- mdy(csse_colnames[indices_date_cols])

  # Preparing the final dataset
  # iso3c and country names
  final_df <- tibble(
    iso3c = unname(final_iso3c)
  )
  # daily data
  csse_daily_data <- csse_df %>%
    select(all_of(indices_date_cols))
  names(csse_daily_data) <- format(csse_data_dates, "%Y-%m-%d")
  # final_df = daily data and iso3c codes together
  final_df %<>%
    bind_cols(csse_daily_data)

  # if required, sum cases by iso3c (country/province)
  if( sum_by_iso3c == TRUE ) {
    final_df %<>%
      group_by(iso3c) %>%
      summarise_all(sum, na.rm = TRUE) %>%
      ungroup()
  }

  # if required, use french governemental values
  if( use_french_gouv_data == TRUE ) {

    # Download the raw data
    datagouv_raw_data <- get_through_system_proxy( "https://www.data.gouv.fr/fr/datasets/r/58aee810-ddd2-4359-85eb-5cfd899cd1ce" )
    stopifnot( status_code(csse_raw_data) == 200 )
    df_datagouv <- fromJSON(content(datagouv_raw_data, type = "text", encoding = "UTF-8"))

    # only the global data of the metropolitan area
    df_fra <- df_datagouv %>%
      filter(.data$code == "FRA") %>%
      filter(.data$sourceType == "ministere-sante")

    # data cleaning
    df_fra %<>%
      select(.data$date, .data$casConfirmes, .data$deces, .data$decesEhpad, .data$gueris) %>%
      mutate_if(is.numeric, function(x) {
        ifelse(is.na(x), 0, x)
      }) %>%
      mutate(deaths = .data$deces + .data$decesEhpad) %>%
      mutate(confirmed = .data$casConfirmes) %>%
      mutate(recovered = .data$gueris) %>%
      mutate(date = as.Date(.data$date)) %>%
      select(.data$date, .data$confirmed, .data$deaths, .data$recovered) %>%
      arrange(.data$date)

    # create csse_like dataset
    csse_base <- tibble(
      date = csse_data_dates
    ) %>%
      left_join(df_fra, by = "date") %>%
      mutate_if(is.numeric, function(x) {
        ifelse(is.na(x), 0, x)
      }) %>%
      select(one_of(c("date", case_type)))

    # cumulative cases. If zero, use the previous value
    ### CAUTION: use a for loop. TO BE MODIFIED ASAP ###
    for(i in seq(from = 2, to = nrow(csse_base), by = 1)) {
      if( csse_base[[case_type]][i] == 0 ) {
        csse_base[[case_type]][i] <- csse_base[[case_type]][i - 1]
      }
    }

    # spread the data as in csse data
    csse_base %<>%
      spread(key = "date", value = case_type)

    csse_fra <- bind_cols(
      tibble(
        iso3c = "FRA",
        csse_base
      )
    )

    # if OK, modify the data
    if( nrow(csse_fra) == 1 && ncol(csse_fra) == ncol(final_df) ) {
      final_df %<>%
        filter(iso3c != "FRA") %>%
        bind_rows(csse_fra)
    }

  }

  iso3c_vector <- final_df$iso3c
  # numerical_final_df = the global results table, with only numerical values,
  # but with the rows in the same order as the iso3c codes
  # and the columns in the same order as the dates.
  numerical_final_df <- final_df %>%
    select(-iso3c)

  result <- csse_data$new(
    case_type = case_type,
    iso3c_country_code = iso3c_vector,
    case_date = csse_data_dates,
    case_value = numerical_final_df
  )
  return(result)

}

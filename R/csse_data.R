#' CSSE data storage and handling class
#' An R6 class allowing to store, manipulate and retrieve CSSE data in a systematized way
#'
#' @docType class
#'
#' @import dplyr
#' @import magrittr
#' @importFrom tidyselect all_of
#' @importFrom rlang .data
#' @importFrom tibble tibble
#' @importFrom R6 R6Class
#'
#' @return Object of \code{\link{R6Class}} with methods for manipulate CSSE data
#'
#' @export
#'
#' @format  An \code{\link{R6Class}} generator object for CSSE data
#' @keywords CSSE data
csse_data <- R6Class(
  classname = "CsseData",
  public = list(
    #' initialize class function
    #'
    #' @param case_type a character string. Possible values : "confirmed", "deaths", "recovered"
    #' @param iso3c_country_code a vector of iso3c codes
    #' @param case_date a vector of dates
    #' @param case_value a table with values by iso3c code (rows) and dates (cols)
    initialize = function(case_type, iso3c_country_code, case_date, case_value) {
      # Checking the parameters
      # only confirmed, deaths or recovered cases
      stopifnot(
        length(case_type) == 1,
        case_type %in% c("confirmed", "deaths", "recovered")
      )
      # at least one country
      stopifnot(
        length(iso3c_country_code) >= 1,
        is.character(iso3c_country_code)
      )
      # data for at least one day
      stopifnot(
        length(case_date) >= 1,
        "Date" %in% class(case_date)
      )
      # case_value: a table with countries in rows and dates in columns
      stopifnot(
        is.data.frame(case_value),
        nrow(case_value) == length(iso3c_country_code),
        ncol(case_value) == length(case_date)
      )

      # store parameters in private variables
      private$case_type <- case_type
      private$iso3c_country_code <- iso3c_country_code
      private$creation_date <- Sys.time()
      # to avoid handling errors, the data is sorted by ascending date
      ind_asc_case_date <- order(case_date)
      private$case_date <- case_date[ind_asc_case_date]
      private$case_value <- case_value %>%
        select(all_of(ind_asc_case_date))

    },

    #' data retrieval function
    #'
    #' @param iso3c_country_code a vector of iso3c codes
    #' @param start_date start date of data
    #' @param end_date end date of data
    #' @param data_type data type : "cumulative" or "daily"
    #' @param by_p100000_incidence a logical value
    #' @param add_suppl_info a logical value: indicates whether the results are reported in frequency or incidence
    get_data = function(iso3c_country_code = NA, start_date = NA, end_date = NA, data_type = "cumulative", by_p100000_incidence = FALSE, add_suppl_info = FALSE) {
      # Checking the parameters
      # iso3c_country_code: either NA or a list of iso codes
      if( !all(is.na(iso3c_country_code)) ) {
        stopifnot(
          length(iso3c_country_code) >= 1,
          is.character(iso3c_country_code)
        )
      }
      # start_date and end_date: either NA or a date
      if( !all(is.na(start_date)) ) {
        stopifnot(
          length(start_date) == 1,
          "Date" %in% class(start_date)
        )
      }
      if( !all(is.na(end_date)) ) {
        stopifnot(
          length(end_date) == 1,
          "Date" %in% class(end_date)
        )
      }
      # data_type: either cumulative cases or daily cases
      stopifnot(
        length(data_type) == 1,
        data_type %in% c("cumulative", "daily")
      )
      # by_p100000_incidence: boolean variable with a unique value
      # Note: if the data is not grouped by iso3c code, the incidences may be erroneous
      stopifnot(
        length(by_p100000_incidence) == 1,
        is.logical(by_p100000_incidence)
      )
      # add_suppl_info: boolean variable with a unique value
      stopifnot(
        length(add_suppl_info) == 1,
        is.logical(add_suppl_info)
      )

      # we retrieve a working copy of the data
      df <- private$case_value

      # filter the data on iso3c codes where applicable
      # The iso3c_country_code vector must have the same length as the number of df lines.
      # If NA is passed as a parameter, the internal values are taken,
      # otherwise the internal values are filtered with the supplied parameter
      if( all(is.na(iso3c_country_code)) ) {
        iso3c_country_code <- private$iso3c_country_code
      } else {
        iso3c_country_code <- private$iso3c_country_code[ private$iso3c_country_code %in% iso3c_country_code ]
      }
      df %<>%
        filter(private$iso3c_country_code %in% iso3c_country_code)
      # If no country or territory is selected, return NULL
      if( nrow(df) == 0 ) {
        return(NULL)
      }
      # If the length of iso3c_country_code differs from the number
      # of lines of df (it should not be possible), returns an error.
      stopifnot(
        length(iso3c_country_code) == nrow(df)
      )

      # if daily cases, data transformation (applies only if more than one column)
      #
      # Assumption: Aggregate data starts at the beginning of the collection
      # To avoid a side-effect, the transformation of cumulative values into daily values
      # takes place before the filter of the data by date.
      #
      # Note: If there are missing dates, the data may be incorrect.
      # The data returned here is the number of new cases from one date to the next,
      # assuming that there is data for all days. Possibly to be improved in a second step
      #
      # Reminder: data is already sorted by ascending date (see initialize function)
      if( data_type == "daily" && ncol(df) > 1 ) {
        # For each line, the number of new cases per day is retrieved.
        # Apply gives a result in columns ==> we transpose
        df %<>%
          apply(1, function(x) {
            n <- length(x)
            a <- x[seq(from = 1, to = n - 1, by = 1)]
            b <- x[seq(from = 2, to = n, by = 1)]
            return( c(x[1], (b - a)) )
          }) %>%
          t() %>%
          as.data.frame() %>%
          tibble()
      }

      # filter the data on start_date and end_date
      # in case of missing values, the extremes are replaced
      # by the extremes in order to have a valid date range
      colnames(df) <- format(private$case_date, "%Y-%m-%d")
      if( is.na(start_date) ) {
        start_date <- min(private$case_date)
      }
      if( is.na(end_date) ) {
        end_date <- max(private$case_date)
      }
      ind_to_keep <- which(
        private$case_date >= start_date & private$case_date <= end_date
      )
      df %<>%
        select(all_of(ind_to_keep))

      # Adding iso3c codes to the results
      df %<>% bind_cols(
        tibble(iso3c = iso3c_country_code)
      )

      # If requested, the results are modified by dividing by the population
      # (incidence instead of frequency).
      if( by_p100000_incidence == TRUE ) {
        # the population column is temporarily added to do the calculations.
        iso3c_pop <- iso3c %>%
          select(iso3c, population)
        df %<>%
          left_join(iso3c_pop, by = "iso3c") %>%
          rowwise() %>%
          ### WARNING: funs() is soft deprecated as of dplyr 0.8.0 ==> NEED TO BE FIXED ASAP ###
          mutate_if(is.numeric, funs((100000 * .) / .data$population)) %>%
          ungroup() %>%
          select(-population)
      }

      # If requested, additional information is added
      if( add_suppl_info == TRUE ) {
        df %<>%
          left_join(iso3c, by = "iso3c")
      }
      return(df)

    },

    #' Function for calculating the doubling time of cases (at a given date)
    #'
    #' @param iso3c_country_code a vector of iso3c codes
    #' @param n_days the number of days used to calculate the growth rate
    #' @param min_cases minimum number of cases to calculate the doubling time
    doubling_time = function(iso3c_country_code = NA, n_days = 7, min_cases = 100) {

      # Checking the parameters
      # iso3c_country_code: either NA or a list of iso codes
      if( !all(is.na(iso3c_country_code)) ) {
        stopifnot(
          length(iso3c_country_code) >= 1,
          is.character(iso3c_country_code)
        )
      }
      # n_days: an integer value greater than 0
      stopifnot(
        is.numeric(n_days),
        length(n_days) == 1,
        n_days > 0
      )
      n_days <- trunc(n_days)
      # min_cases: an integer value greater than 1
      stopifnot(
        is.numeric(min_cases),
        length(min_cases) == 1,
        min_cases > 1
      )
      min_cases <- trunc(min_cases)

      # # we retrieve a working copy of the data
      # df <- private$case_value
      #
      # # filter the data on iso3c codes where applicable
      # # The iso3c_country_code vector must have the same length as the number of df lines.
      # # If NA is passed as a parameter, the internal values are taken,
      # # otherwise the internal values are filtered with the supplied parameter
      # if( all(is.na(iso3c_country_code)) ) {
      #   iso3c_country_code <- private$iso3c_country_code
      # } else {
      #   iso3c_country_code <- private$iso3c_country_code[ private$iso3c_country_code %in% iso3c_country_code ]
      # }
      # df %<>%
      #   filter(private$iso3c_country_code %in% iso3c_country_code)
      # # If no country or territory is selected, return NULL
      # if( nrow(df) == 0 ) {
      #   return(NULL)
      # }
      # # If the length of iso3c_country_code differs from the number
      # # of lines of df (it should not be possible), returns an error.
      # stopifnot(
      #   length(iso3c_country_code) == nrow(df)
      # )

      df <- self$get_data(iso3c_country_code = iso3c_country_code, data_type = "cumulative")
      iso3c_country_code <- df$iso3c
      # If no country or territory is selected, return NULL
      stopifnot(
        !is.null(df),
        nrow(df) > 0
      )
      df %<>%
        select(-iso3c)

      # Let x be the growth rate per day (relative change).
      # The doubling time d is calculated here as d = ln(2)/ln(1 + x).
      # When the growth rate is calculated over n_days days, d = n_days * [n(2)/ln(1 + x)].
      df %<>%
        apply(1, function(x, n_days, min_cases) {
          window_end <- seq(from = n_days, to = length(x), by = 1) # alternative: from = 1, but questionable results in the beginning
          window_start <- sapply(window_end - n_days + 1, max, 1)
          growth_rate <- ifelse(x[window_end] >= min_cases,
                                (x[window_end] - x[window_start]) / x[window_start],
                                NA)
          doubling_time <- n_days * (log(2) / log(1 + growth_rate))
          return(doubling_time)
        }, n_days, min_cases) %>%
        t() %>%
        as.data.frame() %>%
        tibble()

      # Adding iso3c codes
      df %<>%
        bind_cols(
          tibble(iso3c = iso3c_country_code)
        )

      return(df)

    },

    #' Sum or moving average function
    #'
    #' @param iso3c_country_code a vector of iso3c codes
    #' @param n_days size of the sliding window in days
    #' @param rolling_function a character string indicating whether the average or the sum is to be calculated
    #' @param by_p100000_incidence a logical value
    rolling_data = function(iso3c_country_code = NA, n_days = 7, rolling_function = "mean", by_p100000_incidence = FALSE) {

      # Checking the parameters
      # iso3c_country_code: either NA or a list of iso codes
      if( !all(is.na(iso3c_country_code)) ) {
        stopifnot(
          length(iso3c_country_code) >= 1,
          is.character(iso3c_country_code)
        )
      }
      # n_days: an integer value greater than 0
      stopifnot(
        is.numeric(n_days),
        length(n_days) == 1,
        n_days > 0
      )
      n_days <- trunc(n_days)
      # rolling_function: either "mean" or "sum"
      stopifnot(
        is.character(rolling_function),
        length(rolling_function) == 1,
        rolling_function %in% c("mean", "sum")
      )
      # by_p100000_incidence: boolean variable with a unique value
      # Note: if the data is not grouped by iso3c code, the incidences may be erroneous
      stopifnot(
        length(by_p100000_incidence) == 1,
        is.logical(by_p100000_incidence)
      )

      df <- self$get_data(iso3c_country_code = iso3c_country_code, data_type = "daily", by_p100000_incidence = by_p100000_incidence)
      iso3c_country_code <- df$iso3c
      # If no country or territory is selected, return NULL
      stopifnot(
        !is.null(df),
        nrow(df) > 0
      )
      df %<>%
        select(-iso3c)

      # Calculation of the sum or rolling average
      df %<>%
        apply(1, function(x, n_days, rolling_function) {
          window_end <- seq(from = n_days, to = length(x), by = 1)  # alternative: from = 1, but questionable results in the beginning
          # return rolling windows as a list
          window_data <- sapply(window_end, function(i, x, n_days) {
            window_start <- max(i - n_days + 1, 1)
            window_range <- seq(from = window_start, to = i, by = 1)
            return(x[window_range])
          }, x, n_days, simplify = FALSE)
          # apply function to the rolling windows
          if (rolling_function == "mean") {
            result <- sapply(window_data, mean, na.rm = TRUE)
            names(result) <- names(x)[window_end]
            return(result)
          }
          if (rolling_function == "sum") {
            result <- sapply(window_data, sum, na.rm = TRUE)
            names(result) <- names(x)[window_end]
            return(result)
          }
        }, n_days, rolling_function) %>%
        t() %>%
        as.data.frame() %>%
        tibble()

      # Adding iso3c codes
      df %<>%
        bind_cols(
          tibble(iso3c = iso3c_country_code)
        )

      return(df)

    },

#' Extract de main epidemiological indicators
#'
#' @param iso3c_country_code a vector of iso3c codes
#' @param as_of a cut-off date
#'
#' @return a table with the main epidemiological indicators
#' @export
    main_indicators = function(iso3c_country_code = NA, as_of = NA) {

      # get a working copy of the data
      df <- self$get_data(iso3c_country_code = iso3c_country_code, end_date = as_of, add_suppl_info = TRUE)
      df_dt <- self$doubling_time(iso3c_country_code = iso3c_country_code)

      # identifying the column corresponding to the as_of date
      # for df_df
      ind_df_dt_dates <- grep("[0-9]{4}-[0-9]{2}-[0-9]{2}", names(df_dt))
      df_dt_dates <- as.Date(names(df_dt)[ind_df_dt_dates])
      # for df
      ind_df_dates <- grep("[0-9]{4}-[0-9]{2}-[0-9]{2}", names(df))
      df_dates <- as.Date(names(df)[ind_df_dates])

      # update value of as_of
      if( all(is.na(as_of)) ) {
        as_of <- min(
          max(df_dates, na.rm = TRUE),
          max(df_dt_dates, na.rm = TRUE),
          na.rm = TRUE
        )
      }
      stopifnot(
        length(as_of) == 1,
        "Date" %in% class(as_of)
      )

      # get only the necessary data
      df_dt %<>%
        select(one_of("iso3c", as.character(as_of)))
      names(df_dt) <- c("iso3c", "doubling_time")

      # add the doubling time to the general dataset
      df %<>%
        left_join(df_dt, by = "iso3c")


      # exit if not enough data
      stopifnot(
        max(df_dates) - 13 >= min(df_dates)
      )

      # define the "J0" date
      j0 <- max(df_dates)
      ind_j_moins_x <- function(x) {
        which(names(df) == as.character(j0 - x))
      }
      value_j_moins_x <- function(x) {
        df %>% select(all_of(ind_j_moins_x(x))) %>% pull()
      }

      # build the main dataset
      result <- tibble(
        pays = df$country_name_fr,
        iso3c = df$iso3c,
        population = df$population,
        continent = df$geopol_continent,
        cumul_cas_j0 = value_j_moins_x(0),
        cumul_cas_j_moins_1 = value_j_moins_x(1),
        nvx_cas_7_derniers_jours = value_j_moins_x(0) - value_j_moins_x(7),
        nvx_cas_7_jours_precedents = value_j_moins_x(7) - value_j_moins_x(14),
        variation = NA,
        nvx_cas_14_derniers_jours = value_j_moins_x(0) - value_j_moins_x(14),
        taux_incidence_cas_7_derniers_jours = NA,
        taux_incidence_cas_14_derniers_jours = NA,
        temps_doublement = sprintf("%.1f jours", df$doubling_time),
        idh = df$HDI,
        niveau_idh = df$HDI_level,
        cat_pays = NA
      )
      result %<>%
        mutate(variation = (.data$nvx_cas_7_derniers_jours - .data$nvx_cas_7_jours_precedents) / .data$nvx_cas_7_jours_precedents) %>%
        mutate(variation = round(.data$variation * 100)) %>%
        mutate(variation = paste0(as.character(.data$variation), "%"))
      result %<>%
        mutate(taux_incidence_cas_7_derniers_jours = 100000 * .data$nvx_cas_7_derniers_jours / .data$population) %>%
        mutate(taux_incidence_cas_7_derniers_jours = ifelse(is.na(.data$taux_incidence_cas_7_derniers_jours), NA, sprintf("%.0f p.100 000 personnes", .data$taux_incidence_cas_7_derniers_jours)))
      result %<>%
        mutate(taux_incidence_cas_14_derniers_jours = 100000 * .data$nvx_cas_14_derniers_jours / .data$population) %>%
        mutate(taux_incidence_cas_14_derniers_jours = ifelse(is.na(.data$taux_incidence_cas_14_derniers_jours), NA, sprintf("%.0f p.100 000 personnes", .data$taux_incidence_cas_14_derniers_jours)))

      # add WHO data
      oms_transmission_data <- load_oms_data()
      result %<>%
        left_join(oms_transmission_data, by = "iso3c")

      result %<>%
        arrange(.data$pays)

      return(result)

    }

  ),
  private = list(
    case_type = NULL,
    iso3c_country_code = NULL,
    case_date = NULL,
    case_value = NULL,
    creation_date = NA
  )
)




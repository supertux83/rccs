#' Dygraphs time series graph
#' A function to generate dynamic time series graphs
#'
#' @import dplyr
#' @import magrittr
#' @importFrom dygraphs dygraph
#' @importFrom dygraphs dyRangeSelector
#' @importFrom dygraphs dyAxis
#' @importFrom dygraphs dyRoller
#' @importFrom xts xts
#' @importFrom tidyselect all_of
#' @importFrom rlang .data
#'
#' @param csse_data_object a CsseData object created by load_csse
#' @param iso3c_country_code a character vector of iso3c country codes
#' @param by_p100000_incidence a logical value
#' @param plot_title a character string containing the plot title
#' @param roll_period the rolling period (smoothing)
#' @param y_logscale a logical value
#' @param data_type data type : "cumulative" or "daily"
#'
#' @return a dygraph object
#'
#' @export
rccs_dygraph_inc_plot <- function(csse_data_object, iso3c_country_code, by_p100000_incidence = FALSE, plot_title = NA, roll_period = 1, y_logscale = FALSE, data_type = "daily") {

  # csse_data_object: must be a CsseData object
  stopifnot(
    "CsseData" %in% class(csse_data_object)
  )
  # iso3c_country_code: a non-empty character vector
  stopifnot(
    length(iso3c_country_code) >= 1,
    is.character(iso3c_country_code)
  )
  # by_p100000_incidence: boolean variable with a unique value
  # Note: if the data is not grouped by iso3c code, the incidences may be erroneous
  stopifnot(
    length(by_p100000_incidence) == 1,
    is.logical(by_p100000_incidence)
  )
  # plot_title: either NA or a character string
  if( !all(is.na(plot_title)) ) {
    stopifnot(
      length(plot_title) == 1,
      is.character(plot_title)
    )
  } else {
    plot_title <- ""
  }
  # roll_period: a single interger value grater or equal to zero
  stopifnot(
    length(roll_period) == 1,
    is.numeric(roll_period),
    roll_period >= 1
  )
  roll_period <- trunc(roll_period)
  # y_logscale: a single logical value
  stopifnot(
    length(y_logscale) == 1,
    is.logical(y_logscale)
  )
  # data_type: either cumulative cases or daily cases
  stopifnot(
    length(data_type) == 1,
    data_type %in% c("cumulative", "daily")
  )

  # daily cases and log_scale
  if( y_logscale == TRUE && data_type == "daily" ) {
    warning("Using a logarithmic scale to represent daily cases can cause display problems when the number of cases is equal to zero.")
  }

  # y label
  if( by_p100000_incidence == TRUE ) {
    plot_ylab = "Incidence p.100 000 personnes"
  } else {
    plot_ylab = "Nombre de cas"
  }

  # load the data
  df <- csse_data_object$get_data(
    iso3c_country_code = iso3c_country_code,
    data_type = data_type,
    by_p100000_incidence = by_p100000_incidence
  )

  # get the date columns
  ind_date_cols <- grep("[0-9]{4}-[0-9]{2}-[0-9]{2}", names(df))
  date_cols <- as.Date(names(df)[ind_date_cols])

  # get the iso3C codes in the right order
  ordered_iso3c <- df$iso3c

  # rotate the dataset
  ts_df <- df %>%
    select(all_of(ind_date_cols)) %>%
    as.matrix() %>%
    t() %>%
    as.data.frame()
  colnames(ts_df) <- sapply(ordered_iso3c, extended_countrycode, origin = "iso3c", destination = "country.name.fr")
  ts_df %<>%
    mutate(date = date_cols)

  # create the timeseries
  don <- xts(x = select(ts_df, -date), order.by = ts_df$date)

  # create the plot
  fig <- dygraph(don, main = plot_title, ylab = plot_ylab, xlab = "Date") %>%
    dyRangeSelector() %>%
    dyAxis("x", drawGrid = FALSE) %>%
    dyAxis("y", drawGrid = FALSE, logscale = y_logscale) %>%
    dyRoller(rollPeriod = roll_period, showRoller = FALSE)

  return(fig)

}

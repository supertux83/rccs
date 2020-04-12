#' GGplot2 time series graph
#' A function to generate static time series graphs
#'
#' @import dplyr
#' @import magrittr
#' @importFrom tidyselect all_of
#' @importFrom tidyr gather
#' @importFrom rlang .data
#' @importFrom ggplot2 ggplot
#' @importFrom ggplot2 geom_line
#' @importFrom ggplot2 geom_smooth
#' @importFrom ggplot2 scale_y_continuous
#' @importFrom ggplot2 scale_x_date
#' @importFrom ggplot2 theme
#' @importFrom ggplot2 theme_classic
#' @importFrom ggplot2 element_blank
#' @importFrom ggplot2 element_text
#' @importFrom ggplot2 aes_string
#' @importFrom ggplot2 labs
#' @importFrom ggplot2 facet_wrap
#' @importFrom tidyquant geom_ma
#'
#' @param csse_data_object a CsseData object created by load_csse
#' @param iso3c_country_code a character vector of iso3c country codes
#' @param by_p100000_incidence a logical value
#' @param plot_title a character string containing the plot title
#' @param y_logscale a logical value
#' @param data_type data type : "cumulative" or "daily"
#' @param smoothing a logical value
#' @param smoothing_by_ma a logical value
#' @param ma_n_days an integer value (size of the rolling window for averaging)
#' @param start_date either NA or the start date of data
#' @param end_date either NA of the end date of data
#' @param separate_graphs a logicial value
#'
#' @return a ggplot2 object
#' @export
rccs_ggplot_inc_plot <- function(csse_data_object, iso3c_country_code, by_p100000_incidence = FALSE, plot_title = NA, y_logscale = FALSE, data_type = "daily", smoothing = TRUE, smoothing_by_ma = FALSE, ma_n_days = 3, start_date = NA, end_date = NA, separate_graphs = FALSE) {

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
  # smoothing: a single logical value
  stopifnot(
    length(smoothing) == 1,
    is.logical(smoothing)
  )
  # smoothing_by_ma: a single logical value
  stopifnot(
    length(smoothing_by_ma) == 1,
    is.logical(smoothing_by_ma)
  )
  stopifnot(
    length(ma_n_days) == 1,
    is.numeric(ma_n_days),
    ma_n_days >= 1
  )
  ma_n_days <- trunc(ma_n_days)
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
  # separate_graphs: a single logical value
  stopifnot(
    length(separate_graphs) == 1,
    is.logical(separate_graphs)
  )

  # load the data
  df <- csse_data_object$get_data(
    iso3c_country_code = iso3c_country_code,
    data_type = data_type,
    by_p100000_incidence = by_p100000_incidence,
    start_date = start_date,
    end_date = end_date
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

  # prepare the dataset for ggplot
  ts_df %<>% gather("country", "n", -date)

  # y label
  if( by_p100000_incidence == TRUE ) {
    plot_ylab = "Incidence p.100 000 personnes"
  } else {
    plot_ylab = "Nombre de cas"
  }

  # create the plot
  fig <- ggplot(ts_df, aes_string(x = "date", y = "n", colour = "country", fill = "country"))

  # add smoothing if requested
  if( smoothing == TRUE & smoothing_by_ma == TRUE ) {
    fig <- fig + geom_line(size = 0.8, alpha = 0.3) +
      tidyquant::geom_ma(n = ma_n_days, linetype = "twodash", size = 0.8)
  }
  if( smoothing == TRUE & smoothing_by_ma == FALSE ) {
    fig <- fig + geom_line(size = 0.8, alpha = 0.3) +
      geom_smooth(se = FALSE, linetype = "twodash", size = 0.8, method = "gam", formula = y ~ s(x, bs = "cs"))
  }
  if( smoothing == FALSE ) {
    fig <- fig + geom_line(size = 0.8)
  }

  # add log scale if requested
  if ( y_logscale == TRUE ) {
    fig <- fig + scale_y_continuous(trans='log2')
  }

  # add labels and themes
  fig <- fig + scale_x_date(date_labels = "%d/%m", date_breaks = "2 days") +
    labs(
      x = "Jour",
      y = plot_ylab,
      title = plot_title
    ) +
    theme_classic() +
    theme(legend.title = element_blank()) +
    theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
    theme(legend.text=element_text(size=14)) +
    theme(axis.text = element_text(size = 14))

  # distincts graphs if requested
  if( separate_graphs == TRUE ) {
    fig <- fig + facet_wrap(~country, ncol = 2)
  }

  return(fig)

}

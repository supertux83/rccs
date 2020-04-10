#' GGplot2 doubling time graph
#' A function to generate static doubling time graphs
#'
#' @import dplyr
#' @import magrittr
#' @importFrom tidyselect all_of
#' @importFrom tidyr gather
#' @importFrom rlang .data
#' @importFrom ggplot2 ggplot
#' @importFrom ggplot2 geom_line
#' @importFrom ggplot2 geom_smooth
#' @importFrom ggplot2 scale_y_reverse
#' @importFrom ggplot2 scale_x_date
#' @importFrom ggplot2 theme
#' @importFrom ggplot2 theme_classic
#' @importFrom ggplot2 element_blank
#' @importFrom ggplot2 element_text
#' @importFrom ggplot2 aes_string
#' @importFrom ggplot2 labs
#'
#' @param csse_data_object a CsseData object created by load_csse
#' @param iso3c_country_code a character vector of iso3c country codes
#' @param plot_title a character string containing the plot title
#' @param window_size the size of the sliding window used to estimate the doubling time
#' @param min_cases the minimal number of cases to estimate the doubling time
#' @param start_date either NA or the start date of data
#' @param end_date either NA of the end date of data
#'
#' @return a ggplot2 object
#' @export
rccs_ggplot_doubling_time <- function(csse_data_object, iso3c_country_code, plot_title = NA, window_size = 7, min_cases = 100, start_date = NA, end_date = NA) {

  # csse_data_object: must be a CsseData object
  stopifnot(
    "CsseData" %in% class(csse_data_object)
  )
  # iso3c_country_code: a non-empty character vector
  stopifnot(
    length(iso3c_country_code) >= 1,
    is.character(iso3c_country_code)
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
  # window_size: an integer value between 1 and 14
  stopifnot(
    length(window_size) == 1,
    is.numeric(window_size),
    window_size >= 1,
    window_size <= 14
  )
  window_size <- trunc(window_size)
  # min_cases <- a integer greater than 0
  stopifnot(
    length(min_cases) == 1,
    is.numeric(min_cases),
    min_cases >= 1
  )
  min_cases = trunc(min_cases)
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

  # get a local copy of the data
  df <- csse_data_object$doubling_time(n_days = window_size, min_cases = min_cases)

  # prepare the dataset
  df %<>%
    gather(key = "date", value = "doubling_time", -.data$iso3c)
  iso3c_country <- iso3c %>%
    select(.data$iso3c, .data$country_name_fr)
  df %<>%
    left_join(iso3c_country, by = "iso3c")
  df %<>%
    filter(.data$iso3c %in% iso3c_country_code) %>%
    mutate(date = as.Date(.data$date))
  if( !all(is.na(start_date)) ) {
    df %<>%
      filter(.data$date >= start_date)
  }
  if( !all(is.na(end_date)) ) {
    df %<>%
      filter(.data$date <= end_date)
  }

  # create the figure
  fig <- ggplot(df, aes_string(x = "date", y = "doubling_time", group = "country_name_fr", fill = "country_name_fr", colour = "country_name_fr")) +
    geom_line(size = 0.5, alpha = 0.3) +
    geom_smooth(size = 0.8, se = FALSE, linetype = "twodash") +
    scale_y_reverse() +
    scale_x_date(date_labels = "%d/%m", date_breaks = "2 days") +
    labs(
      x = "Date",
      y = "Temps de doublement estim\u00e9 (jours)"
    ) +
    theme_classic() +
    theme(legend.title = element_blank()) +
    theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
    theme(legend.text=element_text(size=14)) +
    theme(axis.text = element_text(size = 14))

  return(fig)

}

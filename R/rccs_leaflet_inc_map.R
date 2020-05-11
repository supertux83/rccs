#' Cumulative incidence Leaflet map
#' A function to generate a dynamic world map
#'
#' @import dplyr
#' @import magrittr
#' @import classInt
#' @importFrom leaflet colorFactor
#' @importFrom htmltools HTML
#' @importFrom rlang .data
#' @importFrom leaflet leaflet
#' @importFrom leaflet addTiles
#' @importFrom leaflet setView
#' @importFrom leaflet addPolygons
#' @importFrom leaflet labelOptions
#' @importFrom leaflet addLegend
#'
#' @param csse_data_object a CsseData object created by load_csse
#' @param n_days cumulative incidence period length
#' @param as_of the cut-off date
#' @param custom_brks custom breaks for legend intervals. Must contains lower an upper bounds.
#'
#' @return a leaflet object
#'
#' @export
rccs_leaflet_inc_map <- function(csse_data_object, n_days = 7, as_of = NA, custom_brks = NA) {

  # csse_data_object: must be a CsseData object
  stopifnot(
    "CsseData" %in% class(csse_data_object)
  )
  # n_days: an single integer value greater than 0
  stopifnot(
    length(n_days) == 1,
    is.numeric(n_days),
    n_days > 0
  )
  n_days = trunc(n_days)
  # as_of: must be NA or a single date
  if( !all(is.na(as_of)) ) {
    stopifnot(
      length(as_of) == 1,
      "Date" %in% class(as_of)
    )
  } else {
    as_of <- NA
  }
  # custom_brks: must be NA or a numeric vector with at least 3 values
  if( !all(is.na(custom_brks)) ) {
    stopifnot(
      is.numeric(custom_brks),
      length(custom_brks) >= 3
    )
  }

  # load data
  df <- csse_data_object$rolling_data(n_days = n_days, by_p100000_incidence = TRUE, rolling_function = "sum")

  # select the cut-off day
  if( is.na(as_of) ) {
    as_of <- names(df) %>%
      as.Date %>%
      max(na.rm = TRUE)
  }
  day_col <- as_of %>%
    format("%Y-%m-%d")

  # prepare the dataset
  df %<>%
    select(one_of("iso3c", day_col))
  names(df) <- c("iso3c", "incidence")
  df %<>%
    mutate(country_fr = sapply(.data$iso3c, extended_countrycode, origin = "iso3c", destination = "country.name.fr"))

  # set the minimum incidence to zero (correct regularizations)
  marks_x <- which(df$incidence < 0)
  df$incidence[marks_x] <- 0

  # find the cut intervals
  if( all(is.na(custom_brks)) ) {
    cuts <- classIntervals(df$incidence, n = 6, style = "quantile", dataPrecision = 1)
    # to avoid duplicate intervals
    cuts_brks <- unique(cuts$brks)
  } else {
    cuts_brks <- sort(custom_brks)
  }
  # prepare the interval labels for the map legend
  cuts_round <- round(cuts_brks, 1)
  n <- seq(from = 1, to = length(cuts_round) - 1, by = 1)
  cuts_labs <- paste0(
    "(",
    cuts_round[n],
    ", ",
    cuts_round[n + 1],
    "]"
  )
  cuts_labs[1] <- gsub("(", "[", cuts_labs[1], fixed = TRUE)
  incidence_levels <- cuts_labs[findInterval(df$incidence, cuts_brks)]
  incidence_levels <- factor(incidence_levels, levels = cuts_labs, ordered = TRUE)
  df %<>%
    mutate(incidence_levels = incidence_levels)

  # include the incidence data in the OGR data
  session_world_spdf <- world_spdf
  session_world_spdf@data %<>%
    mutate(ISO3 = as.character(.data$ISO3)) %>%
    left_join(df, by = c("ISO3" = "iso3c"))

  # set the map palette
  map_palette <- colorFactor(
    palette = "Reds",
    domain = session_world_spdf@data$incidence_levels,
    na.color = "transparent",
    ordered = TRUE,
    levels = levels(incidence_levels)
  )

  # set the map hoover text
  map_text <- paste0(
    session_world_spdf@data$country_fr, "<br />",
    "TI : ", round(session_world_spdf@data$incidence) , " p.100 000 pers"
  ) %>%
    lapply(HTML)

  # create the map itself
  fig <- leaflet(session_world_spdf) %>%
    addTiles() %>%
    setView(lat = 30, lng = 0, zoom = 2) %>%
    addPolygons(
      fillOpacity = 0.5,
      fillColor = ~map_palette(incidence_levels),
      stroke = FALSE,
      label = map_text,
      labelOptions = labelOptions(
        style = list("font-weight" = "normal", padding = "3px 8px"),
        textsize = "13px",
        direction = "auto"
      )
    ) %>%
    addLegend(
      pal = map_palette,
      values = ~incidence_levels,
      opacity = 0.9,
      title = "Incidence p.100 000",
      position = "bottomleft"
    )

  return(fig)

}

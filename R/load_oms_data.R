#' WHO COVID Sitrep data extractor
#'
#' @import dplyr
#' @import magrittr
#' @importFrom rvest html_nodes
#' @importFrom rvest html_attr
#' @importFrom readr write_file
#' @importFrom httr content
#' @importFrom tabulizer extract_tables
#' @importFrom purrr map_dfr
#' @importFrom purrr walk
#' @importFrom tibble as_tibble
#' @importFrom stringr str_detect
#'
#' @return a table with the transmission class for each country
#' @export
load_oms_data <- function() {

  # download the WHO SitRep webpoage
  OMS_SITREP_URL <-
    "https://www.who.int/emergencies/diseases/novel-coronavirus-2019/situation-reports/"
  oms_html <- read_html_through_system_proxy(OMS_SITREP_URL)

  # find possible links on this page
  links_sitrep <- oms_html %>%
    html_nodes(xpath = "//a[@target='_blank' and contains(@href, 'sitrep')]") %>%
    html_attr("href") %>%
    paste0("https://www.who.int", .)

  # identify the last sitrep
  num_sitrep <- gsub(".*sitrep-([0-9]+).*.pdf.*", "\\1", links_sitrep) %>%
    as.numeric()
  ind_last_sitrep <- first(which(num_sitrep == max(num_sitrep)))

  # Download the last SitRep
  last_sitrep <- retry({
    get_through_system_proxy(links_sitrep[ind_last_sitrep])
  })

  # store the sitrep in a temp file
  tmp_storage <- tempfile()
  write_file(content(last_sitrep, type = "raw"), tmp_storage)

  # ask tabulizer to extract the tables
  oms_sitrep_rawtable <- extract_tables(tmp_storage)

  # remove the temp file
  unlink(tmp_storage)

  # concatante all the extracted tables
  oms_sitrep_table <- map_dfr(oms_sitrep_rawtable,
                              function(x) {
                                n <- ncol(x)
                                colnames(x) <- paste0("V", seq(from = 1, to = n, by = 1))
                                x %<>%
                                  as_tibble()
                              })

  # data cleaning
  # identify sub-total or sub-title lines
  oms_regions <- c(
    "territories",
    "erritories",
    "western pacific region",
    "estern pacific region",
    "european region",
    "uropean region",
    "south-east asia region",
    "outh-east asia region",
    "eastern mediterranean region",
    "astern mediterranean region",
    "region of the americas",
    "egion of the americas",
    "african region",
    "frican region",
    "subtotal for all",
    "ubtotal for all"
  )
  oms_sitrep_table %<>% mutate(a_garder = NA)
  oms_sitrep_table$a_garder <-
    apply(oms_sitrep_table, 1, function(x) {
      is_region <- sapply(x, function(y) {
        y <- ifelse(is.na(y), "", y) %>%
          tolower() %>%
          gsub("[^a-z -]", "", .)
        result <- (y %in% oms_regions)
        return(result)
      })
      return(!any(is_region == TRUE))
    })

  # removing the useless top and bottom
  ind_a_garder_false <- which(oms_sitrep_table$a_garder == FALSE)
  oms_sitrep_table$a_garder[seq(from = 1,
                                to = min(ind_a_garder_false),
                                by = 1)] <- FALSE
  oms_sitrep_table$a_garder[seq(
    from = max(ind_a_garder_false),
    to = nrow(oms_sitrep_table),
    by = 1
  )] <- FALSE
  oms_sitrep_table %<>%
    filter(a_garder == TRUE)

  # exceptions are handled on a case-by-case basis (to be maintained)
  # first, manage multiline expression "Community Transmission"
  for(i in seq(from = 1, to = nrow(oms_sitrep_table) - 1, by = 1)) {
    line_txt <- paste0(oms_sitrep_table[i, ], collapse = "")
    if( line_txt == "CommunityTRUE") {
      oms_sitrep_table$a_garder[i] <- FALSE
      oms_sitrep_table$V6[i] <- " Community Transmission "
    }
  }
  oms_sitrep_table %<>%
    filter(a_garder == TRUE)
  #
  # we're looking for a text sequence in column V1
  # If it is found
  # - replace the NA with empty strings
  # - paste the values of each column together in the first row
  # - we put the first line on "to be kept" status
  # - we set the other lines to "to be deleted" status
  multilines_countries <- list(
    c("Lao People's", "Democratic Republic"),
    c("Northern Mariana", "Islands", "(Commonwealth of", "the)"),
    c("United Republic of", "Tanzania"),
    c("Central African", "Republic"),
    c("Democratic Republic", "of the Congo")
  )
  oms_sitrep_table %<>% data.frame(stringsAsFactors = FALSE)
  walk(multilines_countries, function(x) {
    ind_country <- which(oms_sitrep_table$V1 == x[1])
    n_lines <- length(x)
    n_cols <- ncol(oms_sitrep_table)
    if (length(ind_country) > 0) {
      # pour chacune des correspondance de la 1\u00e8re ligne, on v\u00e9rifie la s\u00e9quence
      for (i in 1:length(ind_country)) {
        # on teste qu'il reste suffisament de lignes
        if (ind_country[i] + n_lines - 1 <= nrow(oms_sitrep_table)) {
          target <-
            oms_sitrep_table$V1[seq(from = ind_country[i],
                                    to = ind_country[i] + n_lines - 1,
                                    by = 1)]
          # si il y a correspondance, on applique la fusion
          if (identical(target, x)) {
            oms_sitrep_table[ind_country[i],] <<-
              apply(oms_sitrep_table[seq(from = ind_country[i],
                                         to = ind_country[i] + n_lines - 1,
                                         by = 1),], 2, paste, collapse = " ")


            oms_sitrep_table$a_garder[seq(from = ind_country[i],
                                          to = ind_country[i] + n_lines - 1,
                                          by = 1)] <<- FALSE
            oms_sitrep_table$a_garder[ind_country[i]] <<- TRUE
          }
        }
      }
    }
  })
  oms_sitrep_table %<>%
    filter(a_garder == TRUE)

  # remove bibliographic reference in country names
  oms_sitrep_table$V1 %<>%
    gsub("[[]+[0-9]+[]]+", "", .)

  # identifying the transmission class
  n_cols <- ncol(oms_sitrep_table)
  fulltext <-
    apply(oms_sitrep_table[, 2:n_cols], 1, paste, collapse = " ")
  oms_sitrep_table$transmission <- NA
  # oms_sitrep_table$transmission <-
  #   ifelse(
  #     str_detect(fulltext, "Local"),
  #     "Transmission locale av\u00e9r\u00e9e",
  #     oms_sitrep_table$transmission
  #   )
  # oms_sitrep_table$transmission <-
  #   ifelse(str_detect(fulltext, "Imported"),
  #          "Cas import\u00e9s",
  #          oms_sitrep_table$transmission)
  # oms_sitrep_table$transmission <-
  #   ifelse(
  #     str_detect(fulltext, "investigation"),
  #     "Transmission locale possible",
  #     oms_sitrep_table$transmission
  #   )
  oms_sitrep_table$transmission <-
    ifelse(
      str_detect(fulltext, "Community"),
      "Transmission locale av\u00e9r\u00e9e - Transmission communautaire",
      oms_sitrep_table$transmission
    )
  oms_sitrep_table$transmission <-
    ifelse(
      str_detect(fulltext, "Cluster"),
      "Transmission locale av\u00e9r\u00e9e - Clusters de cas",
      oms_sitrep_table$transmission
    )
  oms_sitrep_table$transmission <-
    ifelse(str_detect(fulltext, "Sporadic"),
           "Cas import\u00e9s ou sporadiques",
           oms_sitrep_table$transmission)
  oms_sitrep_table$transmission <-
    ifelse(
      str_detect(fulltext, "Pending"),
      "En attente r\u00e9ponse du pays",
      oms_sitrep_table$transmission
    )


  # add the iso3c codes
  oms_sitrep_table$iso3c <-
    sapply(
      oms_sitrep_table$V1,
      extended_countrycode,
      origin = "country.name",
      destination = "iso3c"
    )

  # keep only the useful columns
  oms_sitrep_table %<>%
    select(iso3c, transmission) %>%
    filter(!is.na(iso3c))

  return(oms_sitrep_table)

}


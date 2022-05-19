#' @title Start date of survey
#'
#' @description \code{start_date()} takes the wave number and panel
#' and returns the opening date of the survey.
#'
#' @param wave A numeric value denoting the survey wave number.
#' @param panel If wave is 43 or earlier, a character value denoting the survey
#' panel. Valid options are 'A' and 'B'.
#'
#' @return The date of the first day the survey was open.
#'
#' @examples
#' # First date of survey 24A
#' start_date(24, "A")
#'
#' # First date of survey wave 46
#' start_date(46)
#'
#' @export

start_date <- function(wave, panel = NULL){

  # Check argument format

  if(!inherits(wave, c("numeric", "integer"))) {
    stop("The wave number must be in numeric or integer format.")
  }

  if(!is.null(panel) & !inherits(panel, "character")) {
    stop("panel must be a character.")
  }

  # Error if more argument lengths don't match
  if(length(wave) > 1 & length(panel) > 1 & length(wave) != length(panel)) {
    stop("wave and panel arguments must be length 1 or of equal length.")
  }

  # If panel is null, replace with NA
  if(is.null(panel)) panel <- NA_character_

  # Join arguments into tibble
  dat <- tibble::tibble(wave = wave, panel = panel)

  # Check wave valid

  wave_invalid <- dplyr::filter(dat, is.na(wave) | wave <= 0 | wave %% 1 != 0)

  if(nrow(wave_invalid) > 0) {
    stop("wave is missing or invalid.\n",
         "wave must be supplied and must be an integer greater than zero.")
  }

  # Check panel argument valid

  no_panel_needed <- dplyr::filter(dat, !is.na(panel) & wave > 43)

  if(nrow(no_panel_needed) > 0) {
    warning("Panels merged from wave 44 onwards. Supplied panel value(s) will ",
            "not be used.")
  }

  dat %<>% dplyr::mutate(panel = ifelse(wave > 43, NA_character_, panel))

  panel_invalid <-
    dplyr::filter(dat, wave <= 43 & (is.na(panel) | !panel %in% c("A", "B")))

  if(nrow(panel_invalid) > 0) {
    stop("Panel is missing or invalid.\n",
         "Where wave is 43 or earlier, panel must be 'A' or 'B'.")
  }

  # Derive week 1 start date for panel
  dat %<>% dplyr::mutate(w1 = dplyr::case_when(
    is.na(wave) ~ lubridate::dmy(NA),
    is.na(panel) | panel == "A" ~ lubridate::dmy(06082020),
    panel == "B" ~ lubridate::dmy(13082020)
  ))

  purrr::pmap(dat, ~ ..3 + lubridate::weeks((2 * ..1) - 2)) %>% purrr::reduce(c)

}


#' @title Date range of survey
#'
#' @description \code{date_range()} takes the wave number and panel
#' and returns the date range that the survey was open.
#'
#' @param wave A numeric value denoting the survey wave number.
#' @param panel If wave is 43 or earlier, a character value denoting the survey
#' panel. Valid options are 'A' and 'B'.
#' @param suffix Default value is TRUE. If TRUE, the days will be formatted
#' with suffix; e.g. 1st, 2nd.
#'
#' @return The date range that the survey was open.
#'
#' @examples
#' # Date range of survey 24A
#' date_range(24, "A")
#'
#' # Date range of survey wave 46 (without suffixes)
#' date_range(46, suffix = FALSE)
#'
#' @export

date_range <- function(wave, panel = NULL, suffix = TRUE) {

  start_date <- scs::start_date(wave, panel)
  end_date   <- start_date + lubridate::days(6)

  if(suffix) {

    paste(scales::ordinal(lubridate::day(start_date)),
          format(start_date, "%B -"),
          scales::ordinal(lubridate::day(end_date)),
          format(end_date, "%B"))

  } else {

    paste(format(start_date, "%d %B -"), format(end_date, "%d %B"))

  }

}
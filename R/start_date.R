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

  if(!inherits(wave, "numeric")){
    stop("The wave number must be in numeric format.")
  }

  if(wave < 0 | wave %% 1 != 0){
    stop("Wave number must be whole number greater than 0.")
  }

  if(wave >= 44 & !is.null(panel)){
    panel <- NULL
    warning("Panels were merged from wave 44 onwards. ",
            "`panel` value supplied will not be used.")
  }

  if(wave < 44 & any(is.null(panel), !panel %in% c("A", "B"))){
    stop("Panel must be A or B.")
  }

  week_1_start_date <-
    if(is.null(panel)){
      lubridate::dmy(06082020)
    } else if(panel == "A") {
      lubridate::dmy(06082020)
    } else {
      lubridate::dmy(13082020)
    }

  week_1_start_date + lubridate::weeks((2 * wave) - 2)

}

#' @title Date of the first day CoMix survey was open
#'
#' @description \code{start_date()} takes the wave number and panel of
#' a CoMix survey and returns the date of the first day the survey was open.
#'
#' @param wave A numeric value denoting the survey wave number.
#' @param panel A character value denoting the survey panel. Valid
#' options are 'A' and 'B'.
#'
#' @return The date of the first day the survey was open.
#'
#' @examples
#' # First date of Comix survey 24A
#' start_date(24, "A")
#'
#' @export

start_date <- function(wave, panel = c("A", "B")){

  if(!inherits(wave, "numeric")){
    stop("The wave number must be in numeric format.")
  }

  panel <- match.arg(panel)

  week_1_start_date <- dplyr::case_when(
    panel == "A" ~ lubridate::dmy(06082020),
    panel == "B" ~ lubridate::dmy(13082020)
  )

  week_1_start_date + lubridate::weeks((2 * wave) - 2)

}

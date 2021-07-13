#' @title Calculate first active day of CoMix survey.
#'
#' @param wave The number of survey wave in numeric format.
#' @param cohort The survey cohort; either A or B.
#'
#' @return Returns a date corresponding to the day the survey opened.
#' @export
#'
#' @examples
#' # First date of Comix survey 24A
#' start_date(24, "A")

start_date <- function(wave, cohort = c("A", "B")){

  if(class(wave) != "numeric"){
    stop("The wave number must be in numeric format.")
  }

  cohort <- match.arg(cohort)

  week_1_start_date <- dplyr::case_when(
    cohort == "A" ~ lubridate::dmy(06082020),
    cohort == "B" ~ lubridate::dmy(13082020)
  )

  week_1_start_date + lubridate::weeks((2 * wave) - 2)

}

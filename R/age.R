#' @title Calculate age at time of survey
#'
#' @description \code{age} returns a persons age as at the beginning of a
#' survey given their date of birth, survey wave number and panel.
#'
#' @param date_of_birth Date of birth in date format
#' @param wave Wave number
#' @param panel Panel number
#'
#' @return A numeric value corresponding to a persons age as at the
#' beginning of the survey.
#'
#' @examples
#' # Age of someone with DOB 25/12/1980 during survey 25A
#' age(as.Date("1980-12-25"), 25, "A")
#'
#' @export

age <- function(date_of_birth, wave, panel = c("A", "B")){

  if(!inherits(date_of_birth, "Date")){
    stop("date_of_birth must be in date format.")
  }

  if(!inherits(wave, "numeric")){
    stop("The wave number must be in numeric format.")
  }

  panel <- match.arg(panel)

  wave_date <- start_date(wave, panel)

  floor(lubridate::interval(date_of_birth, wave_date) / lubridate::years(1))

}

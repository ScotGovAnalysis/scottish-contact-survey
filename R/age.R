#' @title Assign age groups
#'
#' @description \code{age_group()} takes a numeric vector and assigns each
#' age to the appropriate age group.
#'
#' @param age A vector of numeric values.
#'
#' @return A character vector, where each element is the age group
#' for the corresponding element in \code{age}.
#'
#' @examples
#' age <- c(27, 30, 54, 89)
#' age_group(age)
#'
#' @export

age_group <- function(age){

  if(!inherits(age, c("numeric", "integer"))){
    stop("Ages must have numeric or integer class.")
  }

  if(any(age < 18, na.rm = TRUE)){
    warning(paste("Ages under 18 are coded as NA as",
                  "SCS participants must be 18 or over."))
  }

  dplyr::case_when(
    age %in% 18:29 ~ "18-29",
    age %in% 30:39 ~ "30-39",
    age %in% 40:49 ~ "40-49",
    age %in% 50:59 ~ "50-59",
    age %in% 60:69 ~ "60-69",
    age >= 70 ~ "70+"
  )

}


#' @title Calculate age at time of survey
#'
#' @description `age` returns a persons age (or age group if `grouped = TRUE`)
#' as at the beginning of a survey given their date of birth,
#' survey wave number and panel.
#'
#' @param date_of_birth Date of birth in date format
#' @param wave Wave number
#' @param panel Panel number
#' @param grouped If `TRUE`, will return age group. Default value of `FALSE`
#' will return age.
#'
#' @return A numeric value corresponding to a persons age as at the
#' beginning of the survey.
#'
#' @examples
#' # Age of someone with DOB 25/12/1980 during survey 25A
#' age(as.Date("1980-12-25"), 25, "A")
#'
#' @export

age <- function(date_of_birth, wave, panel = c("A", "B"), grouped = FALSE){

  if(!inherits(date_of_birth, "Date")){
    stop("date_of_birth must be in date format.")
  }

  if(!inherits(wave, "numeric")){
    stop("The wave number must be in numeric format.")
  }

  panel <- match.arg(panel)

  wave_date <- start_date(wave, panel)

  age <-
    floor(lubridate::interval(date_of_birth, wave_date) / lubridate::years(1))

  if(grouped == TRUE) {
    age_group(age)
  } else {
    age
  }

}

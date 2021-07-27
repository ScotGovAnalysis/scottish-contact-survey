#' @title Create age groups
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

  if(!inherits(age, "numeric")){
    stop("Ages must be in numeric format.")
  }

  if(any(age < 18)){
    warning(paste("Ages under 18 are coded as NA as",
                  "CoMix survey participants must be 18 or over."))
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

#' @title Format a postcode
#'
#' @description \code{format_postcode} takes a character string or vector of
#' character strings. It extracts the input values which adhere to the standard
#' UK postcode format (with or without spaces), assigns the appropriate amount
#' of spacing to them and ensures all letters are capitalised.
#'
#' This function is based on the function of the same name from
#' Public Health Scotland's
#' \href{https://github.com/Public-Health-Scotland/phsmethods}{\code{phsmethods}}
#' package.
#'
#' @details The standard UK postcode format (without spaces) is:
#'
#' \itemize{
#' \item 1 or 2 letters, followed by
#' \item 1 number, followed by
#' \item 1 optional letter or number, followed by
#' \item 1 number, followed by
#' \item 2 letters
#' }
#'
#' @param x A character string or vector of character strings. Input values
#' which adhere to the standard UK postcode format may be upper or lower case
#' and will be formatted regardless of existing spacing. Any input values which
#' do not adhere to the standard UK postcode format will generate an NA and a
#' warning message - see \strong{Value} section for more information.
#'
#' @return \code{postcode} returns a character string of length 7. 5 character
#' postcodes have two spaces after the 2nd character; 6 character postcodes
#' have 1 space after the 3rd character; and 7 character postcodes have
#' no spaces.

format_postcode <- function(x) {

  # Set standard postcode format
  pattern <- "^[A-Za-z]{1,2}[0-9][A-Za-z0-9]?[0-9][A-Za-z]{2}$"

  # Strip out all spaces from the input, so they can be added in again later at
  # the appropriate juncture
  x <- stringr::str_remove_all(x, "\\s")

  # Replace postcodes which do not adhere to the standard format with NA (this
  # will also 'replace' NA with NA)
  x <- replace(x, !stringr::str_detect(x, pattern), NA_character_)

  # Capitalise all letters
  x <- toupper(x)

  # Format all valid postcodes to be of length 7, meaning:
  # 5 character postcodes have 2 spaces after the 2nd character;
  # 6 character postcodes have 1 space after the 3rd character;
  # 7 character postcodes have no spaces
  dplyr::case_when(
    is.na(x) ~ NA_character_,
    nchar(x) == 5 ~ sub("(.{2})", "\\1  ", x),
    nchar(x) == 6 ~ sub("(.{3})", "\\1 ", x),
    nchar(x) == 7 ~ x
  )

}
#' \code{comix} package
#'
#' CoMix Scotland R Package
#'
#' @docType package
#' @name comix
#' @importFrom magrittr %>%
#' @importFrom rlang .data
NULL

# Stops notes from appearing in R CMD check because of undefined global
# variable '.'
if(getRversion() >= "2.15.1") utils::globalVariables(".")

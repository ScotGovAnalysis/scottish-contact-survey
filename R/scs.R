#' \code{scs} package
#'
#' Scottish Contact Survey R Package
#'
#' @docType package
#' @name scs
#' @importFrom magrittr %>%
#' @importFrom magrittr %<>%
#' @importFrom rlang .data
NULL

# Stops notes from appearing in R CMD check because of undefined global
# variable '.'
if(getRversion() >= "2.15.1") utils::globalVariables(".")

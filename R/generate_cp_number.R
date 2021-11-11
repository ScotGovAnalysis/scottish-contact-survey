#' @title Generate new CP numbers
#'
#' @description \code{generate_cp_number()} creates a vector of new CP numbers
#' for the required panel.
#'
#' @param active_cp A character vector of existing CP numbers.
#' @param panel A character indicating which panel the numbers are for.
#' @param n An integer indicating how many new CP numbers are required.
#'
#' @return A character vector, length \code{n}, of new CP numbers for
#' \code{panel}.
#'
#' @examples
#' x <- c("CPA0001", "CPA0002", "CPB0001")
#' generate_cp_number(active_cp = x, panel = "A", n = 2)
#'
#' @export

generate_cp_number <- function(active_cp, panel, n){

  # Check format of active_cp
  if(any(!stringr::str_detect(active_cp, "^CP[A-Z]\\d{4}$"), na.rm = TRUE)){
    stop("At least one CP number in incorrect format.")
  }

  # Check n is numeric
  if(!inherits(n, c("numeric", "integer", "double"))){
    stop("n must be in numeric format.")
  }

  # Check panel is correct format
  if(!stringr::str_detect(panel, "^[A-Z]$")){
    stop("panel must be a single upper case character; e.g. 'A'.")
  }

  panel_cp <-
    active_cp[stringr::str_sub(active_cp, 3, 3) == panel & !is.na(active_cp)]

  max_cp <- if(length(panel_cp) > 0){
    panel_cp %>% readr::parse_number() %>% max()
  } else {
    0
  }

  paste0(
    "CP", panel,
    stringr::str_pad(seq(max_cp + 1, max_cp + n, by = 1),
                     width = 4, side = "left", pad = "0")
  )

}
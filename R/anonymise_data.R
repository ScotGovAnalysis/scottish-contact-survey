#' @title Anonymise response data
#'
#' @param data Data frame containing data to anonymise
#' @param dataset_to_anon Determines which variables should be anonymised.
#'
#' \itemize{
#' \item "reg": existing household member names/nicknames
#' \item "resp": existing household member names/nicknames, new household member
#' names/nicknames, contacts names/nicknames
#' }
#'
#' @return Data frame with names/nicknames anonymised.
#'
#' @export

anonymise_data <- function(data, dataset_to_anon = c("resp", "reg")) {

  dataset_to_anon <- match.arg(dataset_to_anon)

  exp_names <- paste0("hm", 1:10, "_name")

  if(dataset_to_anon == "resp") {
    exp_names <- c(exp_names,
                   paste0("new_hm", 1:4, "_name"),
                   paste0("c", 1:30))
  }

  if(any(!exp_names %in% names(data))){
    stop(paste("Following variables missing from data:",
               paste(exp_names[!exp_names %in% names(data)], collapse = ", ")))
  }

  data %>%
    dplyr::mutate(dplyr::across(
        exp_names,
        ~ dplyr::if_else(
          !is.na(.),
          dplyr::cur_column() %>%
            stringr::str_remove("_name") %>%
            toupper() %>%
            stringr::str_replace("NEW_", "New "),
          .
        )
    ))

}

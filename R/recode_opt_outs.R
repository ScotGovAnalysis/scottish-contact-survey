#' @title Recode opt-outs in registration data
#'
#' @description \code{recode_opt_outs()} takes the registration data, recodes
#' status for opt outs and removes all registraion data other than
#' \code{cp_number}, \code{status} and \code{panel}.
#'
#' @param reg_data A dataframe of registraion data.
#' @param cp A character vector of cp_numbers opted out.
#'
#' @return Registration data with opt outs recoded.
#'
#' @export

recode_opt_outs <- function(reg_data, cp){

  reg_data %>%

    # Add flag for opt outs
    dplyr::mutate(opt_out = dplyr::if_else(.data$cp_number %in% cp,
                                           1,
                                           0)) %>%

    # Recode status
    dplyr::mutate(status = dplyr::case_when(
      .data$opt_out == 1 ~ "opt-out",
      TRUE ~ .data$status
    )) %>%

    # Remove all registraion data; keep cp_number, status, panel ONLY
    dplyr::mutate_at(
      dplyr::vars(!c(.data$cp_number, .data$status, .data$panel,
                     .data$date_of_birth, .data$n_household, .data$opt_out,
                     .data$last_updated)),
      ~ dplyr::if_else(.data$opt_out == 1, NA_character_, .)) %>%
    dplyr::mutate_at(
      dplyr::vars(.data$date_of_birth, .data$last_updated),
      ~  dplyr::if_else(.data$opt_out == 1, as.Date(NA), .)) %>%
    dplyr::mutate(
      n_household =
        dplyr::if_else(.data$opt_out == 1,
                       NA_real_,
                       .data$n_household)
      ) %>%

    # Remove opt out flag
    dplyr::select(-.data$opt_out)

}
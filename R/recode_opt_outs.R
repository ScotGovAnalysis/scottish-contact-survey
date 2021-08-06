#' @title Recode opt-outs in registration data
#'
#' @description \code{recode_opt_outs()} takes the registration data, recodes
#' status for opt outs and removes all registraion data other than
#' \code{cp_number}, \code{status} and \code{panel}.
#'
#' @param reg_data A dataframe of registraion data.
#' @param opt_out_data A dataframe of cp_numbers opted out.
#'
#' @return Registration data with opt outs recoded.
#'
#' @export

recode_opt_outs <- function(reg_data, opt_out_data){

  # Add flag for opt outs
  opt_out_data %<>%
    dplyr::select(-c(.data$age, .data$gender)) %>%
    dplyr::mutate(opt_out = 1)

  reg_data %>%

    # Add flag for new opt outs
    dplyr::left_join(opt_out_data, by = "cp_number") %>%

    # Recode status
    dplyr::mutate(status = dplyr::case_when(
      .data$opt_out == 1 ~ "opt-out",
      TRUE ~ .data$status
    )) %>%
    dplyr::select(-.data$opt_out) %>%

    # Remove all registraion data; keep cp_number, status, panel ONLY
    dplyr::mutate_at(
      dplyr::vars(!c(.data$cp_number, .data$status, .data$panel,
                     .data$date_of_birth, .data$n_household)),
      ~ dplyr::if_else(.data$status == "opt-out", NA_character_, .)) %>%
    dplyr::mutate(
      date_of_birth =
        dplyr::if_else(.data$status == "opt-out",
                       as.Date(NA),
                       .data$date_of_birth),
      n_household =
        dplyr::if_else(.data$status == "opt-out",
                       NA_integer_,
                       .data$n_household)
      )

}

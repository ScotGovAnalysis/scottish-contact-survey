#' @title Vaccine Status Changes
#'
#' @description Update registration data with latest vaccine status information.
#'
#' @param reg_data Registration data
#' @param raw_vaccine_data Vaccine data from response data
#'
#' @return Registration data with vaccine status data updated.
#'
#' @export

vaccine_changes <- function(reg_data, raw_vaccine_data){

  if(!all(c("cp_number", "vacc_1", "vacc_2") %in% names(raw_vaccine_data))){
    stop(paste("At least one of the following variables is missing from",
               "raw_vaccine_data: cp_number, vacc_1, vacc_2."))
  }

  changes <- raw_vaccine_data %>%

    # Select rows where vaccine status has changed
    dplyr::filter(
      !is.na(.data$vacc_1) |
        (!is.na(.data$vacc_2) &
           .data$vacc_2 != "Yes, this is still correct.")
    ) %>%

    # Recode responses
    dplyr::mutate_at(
      dplyr::vars(c(.data$vacc_1, .data$vacc_2)),
      ~ dplyr::case_when(
        . == "No" ~ "no doses",
        TRUE ~ stringr::str_extract(., "\\w+ dose(s)?")
      )) %>%

    # Combine vaccine status changes into one column
    dplyr::mutate(
      vaccine_n_doses_new =
        ifelse(!is.na(.data$vacc_1), .data$vacc_1, .data$vacc_2)
    ) %>%
    dplyr::select(.data$cp_number, .data$vaccine_n_doses_new)

  reg_data %>%

    dplyr::left_join(changes, by = "cp_number") %>%
    dplyr::mutate(
      vaccine_n_doses = dplyr::case_when(
        !is.na(.data$vaccine_n_doses_new) ~ .data$vaccine_n_doses_new,
        TRUE ~ .data$vaccine_n_doses
      )
    ) %>%
    dplyr::select(-.data$vaccine_n_doses_new)

}

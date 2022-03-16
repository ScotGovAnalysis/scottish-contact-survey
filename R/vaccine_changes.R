#' @title Vaccine Status Changes
#'
#' @description Get updates to vaccine status from
#' anonymised response data for given \code{wave} and \code{panel} of
#' Scottish Contact Survey (SCS).
#'
#' @param wave Wave of survey data to delete
#' @param panel Panel of survey data to delete
#'
#' @return Data frame of vaccine status changes from responses for given
#' \code{wave} and \code{panel}.
#'
#' @export

vaccine_changes <- function(wave, panel){

  if(!inherits(wave, "numeric")){
    stop("The wave number must be in numeric format.")
  }

  if(any(!panel %in% c("A", "B"))){
    stop("Panel must be A or B.")
  }

  survey <- paste0(wave, panel)

  # Check vaccine data exists for given survey
  if(wave < 33 | survey == "33A") {
    stop("Vaccine data only available from survey 33B onwards.")
  }

  # Check response file exists
  if(!file.exists(
    here::here("data", survey, paste0(survey, "_response-data-anon.rds")))) {
    stop("Required data file does not exist.\n",
         "Expected file: ",
         here::here("data", survey, paste0(survey, "_response-data-anon.rds")))
  }

  # Read in response data
  here::here("data", survey, paste0(survey, "_response-data-anon.rds")) %>%
    readr::read_rds() %>%

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
        stringr::str_detect(., "booster") ~ "three doses",
        stringr::str_detect(., "two doses") ~ "two doses",
        stringr::str_detect(., "one dose") ~ "one dose",
        . == "No" ~ "no doses",
        TRUE ~ NA_character_
      )) %>%

    # Combine vaccine status changes into one column
    dplyr::mutate(
      vaccine_n_doses_new =
        ifelse(!is.na(.data$vacc_1), .data$vacc_1, .data$vacc_2)
    ) %>%
    dplyr::select(.data$cp_number, .data$vaccine_n_doses_new)

}

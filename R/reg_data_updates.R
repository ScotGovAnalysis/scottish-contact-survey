#' @title Updates to registration data
#'
#' @description Get updates to registration data including area of residence,
#' employment/education status, occupation and household income from
#' anonymised response data for given \code{wave} and \code{panel} of
#' Scottish Contact Survey (SCS).
#'
#' @param wave Wave of survey data to delete
#' @param panel Panel of survey data to delete
#'
#' @return Data frame of updated registration data from responses for given
#' \code{wave} and \code{panel}.
#'
#' @export

reg_data_updates <- function(wave, panel){

  survey <- paste0(wave, panel)

  date_updated <- scs::start_date(wave, panel) + lubridate::days(6)

  # Read in response data
  here::here("data", survey, paste0(survey, "_response-data-anon.rds")) %>%
    readr::read_rds() %>%

    # Select responses with updated data
    dplyr::filter(.data$to_update == 1) %>%

    # Get columns with refreshed reg data
    dplyr::select(.data$cp_number, tidyselect::matches("^updated_")) %>%
    dplyr::rename_at(dplyr::vars(-.data$cp_number),
                     ~ stringr::str_remove(., "updated_")) %>%

    # Clean employment / studying
    dplyr::mutate(
      employment = dplyr::case_when(
        stringr::str_detect(.data$employed, "^Employed") ~ .data$employed,
        TRUE ~ .data$employment_status
      ),
      studying = dplyr::case_when(
        !is.na(.data$studying_location) ~ .data$studying_location,
        .data$studying == "Yes" ~ "Prefer not to say",
        TRUE ~ .data$studying_location
      )
    ) %>%

    # Clean postcode
    dplyr::mutate(postcode = postcode(.data$postcode)) %>%

    # Add Local Authority code
    dplyr::left_join(la, by = "local_authority") %>%

    # Add date of last update
    dplyr::mutate(last_updated = date_updated) %>%

    # Reorder columns
    dplyr::select(-.data$employment_status, -.data$employed,
                  -.data$studying_location) %>%
    dplyr::select(.data$cp_number, .data$postcode, .data$local_authority_code,
                  .data$local_authority, .data$employment, .data$studying,
                  tidyselect::everything())

}
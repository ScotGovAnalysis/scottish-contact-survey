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
#' @examples reg_data_updates(36, "A")
#'
#' @export

reg_data_updates <- function(wave, panel){

  survey <- paste0(wave, panel)

  date_updated <- scs::start_date(wave, panel) + lubridate::days(6)

  # Read in response data
  here::here("data", survey, paste0(survey, "_response-data-anon.rds")) %>%
    readr::read_rds() %>%

    # Select responses with updated data
    dplyr::filter(to_update == 1) %>%

    # Get columns with refreshed reg data
    dplyr::select(cp_number, tidyselect::matches("^updated_")) %>%
    dplyr::rename_at(dplyr::vars(-cp_number),
                     ~ stringr::str_remove(., "updated_")) %>%

    # Clean postcode
    dplyr::mutate(postcode = scs::postcode(postcode)) %>%

    # Add Local Authority code
    dplyr::left_join(la, by = "local_authority") %>%
    dplyr::select(cp_number, postcode, local_authority_code,
                  local_authority, everything()) %>%

    # Add date of last update
    dplyr::mutate(last_updated = date_updated)

}
#' @title Updates to registration data
#'
#' @description Update registration data including area of residence,
#' employment/education status, occupation and household income from
#' Scottish Contact Survey (SCS) response data.
#'
#' @param reg_data Registration data
#' @param resp_data Response data
#' @param wave Survey wave
#' @param panel Survey panel
#'
#' @return Registration data with updates from responses.
#'
#' @export

reg_data_updates <- function(reg_data, resp_data, wave, panel){

  if(!inherits(wave, "numeric")){
    stop("The wave number must be in numeric format.")
  }

  if(any(!panel %in% c("A", "B"))){
    stop("Panel must be A or B.")
  }

  date_updated <- start_date(wave, panel) + lubridate::days(6)

  changes <- resp_data %>%

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

  reg_data %>%

    # People without updated info
    dplyr::filter(!.data$cp_number %in% changes$cp_number) %>%

    # People with updated info
    dplyr::bind_rows(
      reg_data %>%
        dplyr::filter(.data$cp_number %in% changes$cp_number) %>%
        dplyr::select(
          -tidyselect::all_of(setdiff(names(changes), "cp_number"))
        ) %>%
        dplyr::left_join(changes, by = "cp_number")
    )

}
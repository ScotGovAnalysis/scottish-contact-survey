#' @title Extract data for survey invites
#'
#' @param reg_data Data frame of registration data
#' @param survey_panel Survey panel. If NULL, will return all active panel
#' members.
#'
#' @return Data frame of participants and registration data required for
#' survey invites.
#'
#' @export

survey_invites <- function(reg_data, survey_panel = NULL) {

  # Select participants in required panel
  if(!is.null(survey_panel)) {
    reg_data %<>% dplyr::filter(.data$panel == survey_panel)
  }

  reg_data %>%

    # Select active participants
    dplyr::filter(.data$status == "active") %>%

    # Add flag for registration data to be updated
    dplyr::mutate(to_update =
                    dplyr::if_else(is.na(.data$last_updated), 1, 0)) %>%

    # Add flag for household members
    dplyr::mutate(household_members =
                    dplyr::if_else(.data$n_household > 1, 1, 0)) %>%

    # Select required variables
    dplyr::select(.data$email, .data$employment, .data$studying,
                  .data$vaccine_n_doses, .data$to_update,
                  tidyselect::contains("_name"), .data$household_members)

}

#' @title Extract data for survey invites
#'
#' @param reg_data Data frame of registration data
#' @param panel Survey panel
#'
#' @return Data frame of participants and registration data required for
#' survey invites.
#'
#' @export

survey_invites <- function(reg_data, panel = c("A", "B")) {

  panel <- match.arg(panel)

  reg_data %>%

    # Select active participants in panel
    dplyr::filter(.data$status == "active" & .data$panel == panel) %>%

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
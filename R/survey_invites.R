#' @title Extract data for survey invites
#'
#' @param reg_data Data frame of registration data
#' @param survey_panel A character to specify which panel to extract
#' invites for. If not specified, all active members will be included.
#'
#' @return Data frame of participants and registration data required for
#' survey invites.
#'
#' @export

survey_invites <- function(reg_data, survey_panel = NULL) {

  # Check reg_data is a data frame / tibble
  if(!"data.frame" %in% class(reg_data)) {
    stop("`reg_data` must be a data frame.")
  }

  exp_names <- c("panel", "status", "last_updated", "n_household",
                 "email", "employment", "studying", "vaccine_n_doses",
                 paste0("hm", 1:10, "_name"))

  # Check reg_data contains required variables
  if(!all(exp_names %in% names(reg_data))) {
    stop("At least one required variable not in `reg_data`. \n",
         "Expected variables: ", paste(exp_names, collapse = ", "))
  }

  # Check panel (if supplied) exists in reg_data
  if(!is.null(survey_panel)) {
    if(!survey_panel %in% unique(reg_data$panel)) {
      stop("There are no registered participants for survey panel ",
           survey_panel, " in `reg_data`. \n",
           "Either supply a valid survey panel or if no panel is supplied, ",
           "all active participants will be selected.")
    }
  }

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

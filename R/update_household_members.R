#' @title Update household members
#'
#' @description \code{update_household_members} takes registration data and
#' updates household members.
#'
#' @param reg_data Data frame of registration data
#' @param to_remove Data frame of household members that have left household
#' @param to_add Data frame of new household members
#'
#' @return Data frame of registration data with household members updated.
#'
#' @export

update_household_members <- function(reg_data,
                                     to_remove = NULL,
                                     to_add = NULL) {

  if(is.null(to_remove) & is.null(to_add)) {
    stop("No household change data supplied: ",
         "`to_remove` and/or `to_add` must be defined.")
  }

  exp_names_add <- c("cp_number", "hm_add", "name", "age",
                     "gender", "occupation", "student")

  exp_names_remove <- c("cp_number", "hm_remove")

  if(!is.null(to_add) &
     any(!exp_names_add %in% names(to_add))) {
    stop("One or more expected variables missing from `to_add`.\n",
         "Missing variables: ",
         paste(setdiff(exp_names_add, names(to_add)),
               collapse = ", "))
  }

  if(!is.null(to_remove) &
     any(!exp_names_remove %in% names(to_remove))) {
    stop("One or more expected variables missing from `to_remove`.\n",
         "Missing variables: ",
         paste(setdiff(exp_names_remove, names(to_remove)),
               collapse = ", "))
  }

  # Select active panel members only
  reg_active <- reg_data %>% dplyr::filter(.data$status == "active")

  # Restructure household data to long format
  reg_updated <- reg_active %>%
    dplyr::select(.data$cp_number, tidyselect::matches("hm\\d{1,2}_")) %>%
    tidyr::pivot_longer(cols = tidyselect::matches("^hm\\d{1,2}_"),
                        names_to = c("hm", ".value"),
                        names_sep = "_",
                        values_drop_na = TRUE)

  # Remove people who have left household

  if(!is.null(to_remove)) {
    reg_updated %<>%
      dplyr::left_join(to_remove %>% dplyr::mutate(remove = 1),
                       by = c("cp_number", "hm" = "hm_remove")) %>%
      dplyr::filter(is.na(.data$remove)) %>%
      dplyr::select(-.data$remove)
  }

  # Add new household members

  if(!is.null(to_add)) {
    reg_updated %<>% dplyr::bind_rows(to_add %>% dplyr::select(-.data$hm_add))
  }

  reg_updated %<>%

    # Renumber household members
    dplyr::group_by(.data$cp_number) %>%
    dplyr::mutate(hm = paste0("hm", dplyr::row_number()))%>%
    dplyr::ungroup() %>%

    # Complete for 10 household members per participant
    dplyr::group_by(.data$cp_number) %>%
    tidyr::complete(hm = paste0("hm", 1:10)) %>%
    dplyr::ungroup()


  # Check if any panel members have more than 10 household members

  hm_over10 <-
    reg_updated %>%
    dplyr::filter(!is.na(.data$name)) %>%
    dplyr::mutate(hm = as.numeric(stringr::str_extract(.data$hm, "\\d+"))) %>%
    dplyr::group_by(.data$cp_number) %>%
    dplyr::summarise(hm_over10 = (max(.data$hm) > 10) * 1, .groups = "drop")

  n_over10 <- sum(hm_over10$hm_over10)

  if(n_over10 > 0) {
    rlang::warn(c(
      paste(n_over10, "panel members have more than 10 household members."),
      i = paste("The maximum number of household members that can be recorded",
                "in registration data is 10."),
      i = paste("New household members that have been added and cause the",
                "household member count to exceed 10 will not be recorded.")
    ))
  }

  reg_updated %<>%
    dplyr::filter(stringr::str_detect(.data$hm, "^hm([1-9]|10)$"))


  # Restructure data to wide format; one row per participant

  reg_updated %<>%

    tidyr::pivot_wider(names_from = .data$hm,
                       values_from = .data$name:.data$student) %>%

    # Fix value names
    dplyr::rename_with(
      .cols = tidyselect::matches("^.*_hm\\d{1,2}$"),
      .fn = ~ stringr::str_replace(., "(.*)_(.*)", "\\2_\\1")
    ) %>%

    # Reorder columns from HM1 to HM10
    dplyr::select(.data$cp_number,
                  purrr::pmap_chr(expand.grid(c("name", "age", "gender",
                                                "occupation", "student"),
                                              1:10),
                                  ~ paste0("hm", .y, "_", .x)))

  reg_active %>%

    # Remove exisiting household member data
    dplyr::select(-tidyselect::matches("^hm\\d{1,2}_")) %>%

    # Add updated household member data
    dplyr::left_join(reg_updated, by = "cp_number") %>%

    # Recalculate n_household
    dplyr::mutate(
      n_household =
        dplyr::select(., tidyselect::matches("^hm\\d{1,2}_name$")) %>%
        dplyr::mutate_all(~ ifelse(!is.na(.), 1, 0)) %>%
        purrr::reduce(`+`) %>%
        magrittr::add(1)
    ) %>%

    dplyr::bind_rows(
      reg_data %>% dplyr::filter(.data$status != "active")
    )

}

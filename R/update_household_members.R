#' @title Update household members
#'
#' @description \code{update_household_members} takes registration data and
#' updates household members.
#'
#' @param reg_data Data frame of registration data
#' @param to_remove Data frame of household members that have left household
#' @param to_add Data frame of new household members
#'
#' @return A numeric value corresponding to a persons age as at the
#' beginning of the survey.
#'
#' @export

update_household_members <- function(reg_data,
                                     to_remove = NULL,
                                     to_add = NULL) {

  if(is.null(to_remove) & is.null(to_add)) {
    stop("No household change data supplied: ",
         "`to_remove` and/or `to_add` must be defined.")
  }

  # Select active panel members only
  reg_active <- reg_data %>% dplyr::filter(status == "active")

  # Restructure household data to long format
  reg_updated <- reg_active %>%
    dplyr::select(cp_number, tidyselect::matches("hm\\d{1,2}_")) %>%
    tidyr::pivot_longer(cols = matches("^hm\\d{1,2}_"),
                        names_to = c("hm", ".value"),
                        names_sep = "_",
                        values_drop_na = TRUE)

  # Remove people who have left household

  if(!is.null(to_remove)) {
    reg_updated %<>%
      dplyr::left_join(remove %>% dplyr::mutate(remove = 1),
                       by = c("cp_number", "hm" = "hm_remove")) %>%
      dplyr::filter(is.na(remove)) %>%
      dplyr::select(-remove)
  }

  # Add new household members

  if(!is.null(to_add)) {
    reg_updated %<>% dplyr::bind_rows(to_add %>% dplyr::select(-hm_add))
  }

  reg_updated %<>%

    # Renumber household members
    dplyr::group_by(cp_number) %>%
    dplyr::mutate(hm = paste0("hm", dplyr::row_number()))%>%
    dplyr::ungroup() %>%

    # Complete for 10 household members per participant
    dplyr::group_by(cp_number) %>%
    tidyr::complete(hm = paste0("hm", 1:10)) %>%
    dplyr::ungroup() %>%

    # Restructure data to wide format; one row per participant
    tidyr::pivot_wider(names_from = hm,
                       values_from = name:student) %>%

    # Fix value names
    dplyr::rename_at(dplyr::vars(tidyselect::matches("^.*_hm\\d{1,2}$")),
                     ~ stringr::str_replace(., "(.*)_(.*)", "\\2_\\1")) %>%

    # Reorder columns from HM1 to HM10
    dplyr::select(cp_number,
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
      reg %>% dplyr::filter(status != "active")
    )

}

#' @title Add empty columns to data
#'
#' @param data Data to add columns to
#' @param n_to_add Number of columns to add
#' @param after Name of columns to add columns after

add_cols <- function(data, n_to_add, after) {

  n_cols  <- ncol(data)
  n_after <- which(names(data) == after)

  max_temp <-
    data %>%
    dplyr::select(tidyselect::matches("^temp_\\d+$")) %>%
    names() %>%
    stringr::str_extract("\\d+$") %>%
    as.numeric()

  first_temp <- ifelse(length(max_temp) > 0, max(max_temp) + 1, 1)

  new_names <- sprintf("temp_%d", first_temp:(first_temp + n_to_add - 1))

  data %<>% magrittr::inset(new_names, value = NA)

  if(n_cols == n_after) data else {
    data %<>%
      dplyr::select(1:n_after,
                    (n_cols + 1):(n_cols + n_to_add),
                    (n_after + 1):n_cols)
  }

}


#' @title Reformat anonymised response data
#'
#' @param anon_resp_data Data frame of anonymised response data.
#' @param names Column names for reformatted data.
#'
#' @return Reformatted data frame of anonymised response data.
#'
#' @export

reformat_anon_resp <- function(anon_resp_data, names) {

  if(!inherits(names, "character")){
    stop("Names must be in character format.")
  }

  anon_resp_data %>%

    # Remove some columns
    dplyr::select(
      -tidyselect::any_of(c(
        "in_scotland", "vaccine", "to_update",
        "household_members", "time_since_covid_unconfirmed"
      )),
      -tidyselect::matches("^vacc_[12]$"),
      -tidyselect::matches("lateral_flow"),
      -tidyselect::matches("^(hm\\d{1,2}_)?test_positive"),
      -tidyselect::matches("^updated_"),
      -tidyselect::matches("^visit_healthcare_"),
      -tidyselect::matches("^covid_(un)?confirmed")
    ) %>%

    # Add empty columns and rearrange
    tibble::add_column(hm11_change = NA, .after = 16) %>%
    magrittr::inset(sprintf("temp_%d", 1:245), value = NA) %>%
    magrittr::extract(, c(1:39, 1430:1674, 40:1429)) %>%
    tibble::add_column(hm15_contact = NA, .after = 332) %>%
    magrittr::inset(sprintf("temp_%d", 246:(246+26)), value = NA) %>%
    magrittr::extract(, c(1:741, 1676:1702, 742:1675)) %>%
    magrittr::extract(, c(1:1690, 1693:1702, 1691:1692)) %>%
    tibble::add_column(hm11_name = NA, employment_1_0 = NA, .after = 1700) %>%
    tibble::add_column(children_1 = NA, children_2 = NA, .after = 1703) %>%
    magrittr::inset(sprintf("end_%d", 1:56), value = NA) %>%

    # Rename variables for controller script
    magrittr::set_names(names)

}


#' @title Reformat anonymised registration data
#'
#' @param anon_reg_data Data frame of anonymised registration data.
#' @param names Column names for reformatted data.
#' @param wave Survey wave.
#' @param panel Survey panel.
#'
#' @return Reformatted data frame of anonymised registration data.
#'
#' @export

reformat_anon_reg <- function(anon_reg_data, names, wave, panel = NULL) {

  if(!inherits(names, "character")){
    stop("Names must be in character format.")
  }

  if(!inherits(wave, "numeric")){
    stop("The wave number must be in numeric format.")
  }

  if(wave < 44 & is.null(panel)){
    stop("For waves 43 and earlier, `cur_panel` must be provided.")
  }

  if(wave >= 44 & !is.null(panel)){
    panel <- NULL
    warning("Panels were merged from wave 44 onwards. ",
            "`cur_panel` value supplied will not be used.")
  }

  if(wave < 44 & any(is.null(panel), !panel %in% c("A", "B"))){
    stop("Panel must be A or B.")
  }

  anon_reg_data %>%

    # Add missing columns and reorder
    dplyr::select(-.data$status, -.data$panel, -.data$local_authority_code,
                  -tidyselect::contains("vaccine"), -.data$last_updated) %>%
    tibble::add_column(occupation_3 = NA, .after = "occupation_2") %>%
    tibble::add_column(high_risk = NA, medium_risk = NA,
                       .after = "other_ethnicity") %>%
    tibble::add_column(hh_changes = NA, .after = "cp_number") %>%
    tibble::add_column(ethnicity2 = NA, .after = "ethnicity") %>%
    tibble::add_column(studying_yn = NA, .after = "employment") %>%
    tibble::add_column(also_employed = NA, furloughed = NA, .after = "studying") %>%

    dplyr::select(.data$cp_number:.data$n_household,
                  tidyselect::matches("hm\\d{1,2}_name"),
                  tidyselect::everything()) %>%
    tibble::add_column(
      `12.1` = NA, `12.2` = NA, `12.3` = NA,
      `12.4` = NA, `12.5` = NA, `12.6` = NA,
      `12.7` = NA, `12.8` = NA, `12.9` = NA,
      `12.10` = NA, `12.11` = NA, `12.12` = NA,
      .after = "hm10_name"
    ) %>%
    dplyr::select(.data$cp_number:.data$`12.12`,
           .data$employment:.data$total_household_income,
           tidyselect::everything()) %>%
    dplyr::mutate(date_of_birth = age(.data$date_of_birth, wave, panel)) %>%

    # Rename variables
    magrittr::set_names(names)

}

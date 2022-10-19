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
    data %>%
      dplyr::select(1:n_after,
                    (n_cols + 1):(n_cols + n_to_add),
                    (n_after + 1):n_cols)
  }

}


#' @title Assign names
#'
#' @param data Data frame to assign names to
#' @param names Vector of names to assign

assign_names <- function(data, names) {

  if(!(is.vector(names) & is.character(names))) {
    stop("`names` must be a character vector.")
  }

  if(!is.data.frame(data)) {
    stop("`data` must be a data frame.")
  }

  # Check
  diff <- ncol(data) - length(names)

  issue <- dplyr::case_when(
    diff == 1  ~ " more column",
    diff > 1   ~ " more columns",
    diff == -1 ~ " fewer column",
    diff < -1  ~ " fewer columns"
  )

  if(diff != 0) {
    stop("Dataset has ", abs(diff), issue, " than names to assign.")
  }

  data %>% magrittr::set_names(names)

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

  # Remove some columns
  anon_resp_data %<>%
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
    )

  # Add empty columns
  anon_resp_data %<>%
    add_cols(1, after = "hm10_change") %>%
    add_cols(245, after = "new_hm4_student") %>%
    add_cols(1, after = "hm14_contact")

  for(i in 1:14) {
    anon_resp_data %<>%
      add_cols(10, after = paste0("hm", i, "_other")) %>%
      add_cols(1, after = paste0("hm", i, "_outside"))
  }

  anon_resp_data %<>%
    add_cols(27, after = "temp_401")

  for(i in 1:30) {
    anon_resp_data %<>%
      add_cols(1, after = paste0("c", i, "_gender")) %>%
      add_cols(10, after = paste0("c", i, "_other")) %>%
      add_cols(1, after = paste0("c", i, "_outside"))
  }

  anon_resp_data %<>%
    add_cols(1, after = "work_physical_65_over") %>%
    add_cols(1, after = "education_physical_65_over") %>%
    add_cols(1, after = "other_physical_65_over") %>%
    dplyr::select(1:(ncol(.) - 12),
                  (ncol(.) - 9):ncol(.),
                  .data$employment,
                  .data$studying) %>%
    add_cols(2, after = "hm10_name") %>%
    add_cols(2, after = "employment") %>%
    add_cols(56, after = "studying")

  # Rename variables
  assign_names(anon_resp_data, names)

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

  anon_reg_data %<>%

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
    dplyr::mutate(date_of_birth = age(.data$date_of_birth, wave, panel))

  # Rename variables
  assign_names(anon_reg_data, names)

}

#' @title Remove opt-outs from active panel in registration data
#'
#' @description \code{remove_opt_outs()} takes the registration data, recodes
#' status for opt outs and removes all registraion data other than
#' \code{cp_number}, \code{status} and \code{panel}.
#'
#' @param reg_data A dataframe of registration data.
#' @param cp A character vector of cp_numbers opted out.
#'
#' @return Registration data with opt outs recoded.
#'
#' @export

remove_opt_outs <- function(reg_data, cp){

  # Check reg_data is a data frame / tibble
  if(!"data.frame" %in% class(reg_data)) {
    stop("`reg_data` must be a data frame.")
  }

  # Check cp_number and status are in reg_data
  if(!all(c("cp_number", "status") %in% names(reg_data))) {
    stop("cp_number and/or status missing from `reg_data`.")
  }

  # Error/Warning if all/any CP numbers don't exist in reg_data
  no_match <- cp[!cp %in% reg_data$cp_number]

  if(length(no_match) >= 1) {
    warning(length(no_match), " of ", length(cp), " CP numbers to opt out ",
            "do not have a match in reg_data.",
            ifelse(length(no_match) == length(cp),
                   "\n reg_data will be returned unchanged.",
                   ""))
  }

  reg_data %>%

    # Add flag for opt outs
    dplyr::mutate(opt_out = dplyr::if_else(.data$cp_number %in% cp,
                                           1,
                                           0)) %>%

    # Recode status
    dplyr::mutate(status = dplyr::case_when(
      .data$opt_out == 1 ~ "opt-out",
      TRUE ~ .data$status
    )) %>%

    # Remove all registraion data; keep cp_number, status, panel ONLY
    dplyr::mutate(
      dplyr::across(
        !tidyselect::any_of(c("cp_number", "status", "panel", "date_of_birth",
                              "n_household", "opt_out", "last_updated")),
        ~ dplyr::if_else(.data$opt_out == 1, NA_character_, .)
      ),
      dplyr::across(
        tidyselect::any_of(c("date_of_birth", "last_updated")),
        ~ dplyr::if_else(.data$opt_out == 1, as.Date(NA), .)
      ),
      dplyr::across(
        tidyselect::any_of("n_household"),
        ~ dplyr::if_else(.data$opt_out == 1, NA_real_, .)
      )
    ) %>%

    # Remove opt out flag
    dplyr::select(-.data$opt_out)

}


#' @title Replace opt outs
#'
#' @description \code{replace_opt_outs} replaces opt outs from
#' the reserve list matched by age group and gender
#' (or age group only if no complete matches).
#'
#' @param reg_data Data frame of registration data.
#' @param opt_out_data Data frame containing an age and gender breakdown of
#' opt outs.
#' @param cur_wave Current wave of the survey
#' @param cur_panel Current panel of the survey; panel to add replacements to.
#'
#' @return Data frame of registration data with opt outs replaced.
#'
#' @export

replace_opt_outs <- function(reg_data,
                             opt_out_data,
                             cur_wave,
                             cur_panel = NULL){

  # Check reg_data is a data frame / tibble
  if(!"data.frame" %in% class(reg_data)) {
    stop("`reg_data` must be a data frame.")
  }

  # Check required variables exist in reg_data
  if(!all(c("cp_number", "email", "status", "date_of_birth", "gender") %in%
          names(reg_data))){
    stop(paste("At least one of the following variables is missing from",
               "reg_data: cp_number, email, status, date_of_birth, gender."))
  }

  # Check reg_data is a data frame / tibble
  if(!"data.frame" %in% class(opt_out_data)) {
    stop("`opt_out_data` must be a data frame.")
  }

  # Check required variables exist in opt_out_data
  if(!all(c("age_group", "gender", "n_opt_outs") %in% names(opt_out_data))){
    stop(paste("At least one of the following variables is missing from",
               "opt_out_data: age_group, gender, n_opt_outs."))
  }

  # Validate panel
  if(cur_wave < 44 & is.null(cur_panel)){
    stop("For waves 43 and earlier, `cur_panel` must be provided.")
  }

  if(cur_wave >= 44 & !is.null(cur_panel)){
    cur_panel <- NULL
    warning("Panels were merged from wave 44 onwards. ",
            "`cur_panel` value supplied will not be used.")
  }

  if(cur_wave < 44 & any(is.null(cur_panel), !cur_panel %in% c("A", "B"))){
    stop("Panel must be A or B.")
  }

  reserve_data <-
    reg_data %>%
    dplyr::mutate(age_group = age(.data$date_of_birth,
                                  cur_wave,
                                  cur_panel,
                                  grouped = TRUE)) %>%
    dplyr::filter(.data$status == "reserve") %>%
    dplyr::select(.data$email, .data$age_group, .data$gender)

  opt_out_data %<>%
    dplyr::select(.data$age_group, .data$gender, .data$n_opt_outs)

  set.seed(8828)

  match <-
    reserve_data %>%
    dplyr::inner_join(opt_out_data, by = c("age_group", "gender")) %>%
    dplyr::group_by(.data$age_group, .data$gender) %>%
    dplyr::mutate(n_available = dplyr::n(),
                  sample_size = min(.data$n_opt_outs, .data$n_available)) %>%
    dplyr::sample_n(size = max(.data$sample_size)) %>%
    dplyr::ungroup() %>%
    dplyr::select(.data$email, .data$age_group, .data$gender)

  opt_out_data %<>%
    dplyr::left_join(
      match %>%
        dplyr::group_by(.data$age_group, .data$gender) %>%
        dplyr::summarise(n_replaced = dplyr::n()),
      by = c("age_group", "gender")
    ) %>%
    tidyr::replace_na(list(n_replaced = 0))

  opt_outs_leftover <-
    opt_out_data %>%
    dplyr::mutate(n_remaining = .data$n_opt_outs - .data$n_replaced) %>%
    dplyr::filter(.data$n_remaining != 0) %>%
    dplyr::select(.data$age_group, .data$gender, .data$n_remaining)

  match_age_only <-
    reserve_data %>%
    # Remove those already sampled
    dplyr::filter(!.data$email %in% match$email) %>%
    dplyr::inner_join(opt_outs_leftover %>%
                        dplyr::group_by(.data$age_group) %>%
                        dplyr::summarise(n_remaining = sum(.data$n_remaining)),
                      by = "age_group")

  if(nrow(match_age_only) > 0){

    set.seed(8828)

    match_age_only %<>%
      dplyr::group_by(.data$age_group) %>%
      dplyr::mutate(n_available = dplyr::n(),
                    sample_size = min(.data$n_remaining, .data$n_available)) %>%
      dplyr::sample_n(size = max(.data$sample_size)) %>%
      dplyr::ungroup() %>%
      dplyr::select(.data$email, .data$age_group, .data$gender)

  }

  new_panel <- ifelse(is.null(cur_panel), "Z", cur_panel)

  replace <-
    dplyr::bind_rows(match, match_age_only) %>%
    dplyr::mutate(new_cp = generate_cp_number(reg_data$cp_number,
                                              new_panel,
                                              n = nrow(.)))

  replace_summary <-
    opt_out_data %>%
    dplyr::left_join(
      match_age_only %>%
        dplyr::count(.data$age_group, .data$gender) %>%
        dplyr::mutate(gender = dplyr::case_when(
          .data$gender == "Female" ~ "Male",
          .data$gender == "Male" ~ "Female"
        )),
      by = c("age_group", "gender")
    ) %>%
    tidyr::replace_na(list(n = 0)) %>%
    dplyr::rename(age_gender_match = .data$n_replaced,
                  age_only_match   = .data$n) %>%
    dplyr::mutate(
      unmatched =
        .data$n_opt_outs - .data$age_gender_match - .data$age_only_match
    )

  message(
    "Summary of opt outs and replacements: \n",
    "  Total opt outs: ",
    sum(replace_summary$n_opt_outs), "\n",
    "  Replaced by age group and gender match: ",
    sum(replace_summary$age_gender_match), "\n",
    "  Replaced by age group match: ",
    sum(replace_summary$age_only_match), "\n",
    "  Not replaced: ",
    sum(replace_summary$unmatched)
  )

  reg_data %>%
    dplyr::left_join(
      replace %>% dplyr::select(.data$email, .data$new_cp),
      by = "email"
    ) %>%
    dplyr::mutate(
      panel = dplyr::if_else(!is.na(.data$new_cp), new_panel, .data$panel),
      status = dplyr::if_else(!is.na(.data$new_cp), "active", .data$status),
      cp_number = dplyr::if_else(!is.na(.data$new_cp),
                                 .data$new_cp,
                                 .data$cp_number)
    ) %>%
    dplyr::select(-.data$new_cp)

}

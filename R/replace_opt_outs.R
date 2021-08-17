#' @title Replace opt outs
#'
#' @description \code{replace_opt_outs} finds replacements for opt outs from
#' the reserve list matched by age group and gender
#' (or age group only if no complete matches).
#'
#' @param reserve_data A data frame containing an age and gender breakdown of
#' the reserve list.
#' @param opt_out_data A data frame containing an age and gender breakdown of
#' opt outs.
#'
#' @return A character vector of email addresses to replace opt outs.
#'
#' @export

replace_opt_outs <- function(reserve_data, opt_out_data){

  if(!all(c("email", "age_group", "gender") %in% names(reserve_data))){
    stop(paste("At least one of the following variables is missing from",
               "reserve_data: email, age_group, gender."))
  }

  if(!all(c("age_group", "gender", "n_opt_outs") %in% names(opt_out_data))){
    stop(paste("At least one of the following variables is missing from",
               "opt_out_data: age_group, gender, n_opt_outs."))
  }

  reserve_data %<>%
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
    dplyr::select(.data$email, .data$age_group, .data$gender)

  opt_outs_leftover <-
    opt_out_data %>%
    dplyr::left_join(
      match %>%
        dplyr::group_by(.data$age_group, .data$gender) %>%
        dplyr::summarise(n_replaced = dplyr::n()),
      by = c("age_group", "gender")
    ) %>%
    tidyr::replace_na(list(n_replaced = 0)) %>%
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
    match_age_only %<>%
      dplyr::group_by(.data$age_group) %>%
      dplyr::mutate(n_available = dplyr::n(),
                    sample_size = min(.data$n_remaining, .data$n_available)) %>%
      dplyr::sample_n(size = max(.data$sample_size)) %>%
      dplyr::select(.data$email, .data$age_group, .data$gender)
  }

  replace <- dplyr::bind_rows(match, match_age_only)

  replace_summary <-
    opt_out_data %>%
    dplyr::full_join(replace %>%
                       dplyr::group_by(.data$age_group, .data$gender) %>%
                       dplyr::summarise(n_replace = dplyr::n()),
                     by = c("age_group", "gender")) %>%
    tidyr::replace_na(list(n_opt_outs = 0, n_replace = 0))

  message("Summary of opt outs and replacements:")
  print(replace_summary)

  replace %>% dplyr::pull(.data$email)

}

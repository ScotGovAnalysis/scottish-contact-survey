#' @title Anonymise registration data
#'
#' @param data Data frame of registration data
#'
#' @return Data frame of registration data with names/nicknames for household
#' members anonymised.
#'
#' @export

anon_reg_data <- function(data){

  exp_names <- paste0("hm", 1:10, "_name")

  if(any(!exp_names %in% names(data))){
    stop(paste("Following variables missing from data:",
               exp_names[!exp_names %in% names(data)]))
  }

  data %>%

    dplyr::mutate_at(
      dplyr::vars(tidyselect::matches("^hm([1-9]|1[0-1])_name$")),
      ~ as.character(.)
    ) %>%

    # Anonymise household members
    dplyr::mutate(
      hm1_name = dplyr::if_else(!is.na(.data$hm1_name), "HM1", .data$hm1_name),
      hm2_name = dplyr::if_else(!is.na(.data$hm2_name), "HM2", .data$hm2_name),
      hm3_name = dplyr::if_else(!is.na(.data$hm3_name), "HM3", .data$hm3_name),
      hm4_name = dplyr::if_else(!is.na(.data$hm4_name), "HM4", .data$hm4_name),
      hm5_name = dplyr::if_else(!is.na(.data$hm5_name), "HM5", .data$hm5_name),
      hm6_name = dplyr::if_else(!is.na(.data$hm6_name), "HM6", .data$hm6_name),
      hm7_name = dplyr::if_else(!is.na(.data$hm7_name), "HM7", .data$hm7_name),
      hm8_name = dplyr::if_else(!is.na(.data$hm8_name), "HM8", .data$hm8_name),
      hm9_name = dplyr::if_else(!is.na(.data$hm9_name), "HM9", .data$hm9_name),
      hm10_name =
        dplyr::if_else(!is.na(.data$hm10_name), "HM10", .data$hm10_name)
    )

}

#' @title Anonymise response data
#'
#' @param data Data frame of survey responses
#'
#' @return Data frame of survey responses with names/nicknames for new household
#' members, existing household members and contacts anonymised.
#'
#' @export

anon_response_data <- function(data){

  exp_names <- c(paste0("new_hm", 1:4, "_name"),
                 paste0("c", 1:30),
                 paste0("hm", 1:11, "_name"))

  if(any(!exp_names %in% names(data))){
    stop(paste("Following variables missing from data:",
               exp_names[!exp_names %in% names(data)]))
  }

  data %>%

    dplyr::mutate_at(
      dplyr::vars(c(tidyselect::matches("^new_hm[1-4]_name$"),
                    tidyselect::matches("^c([1-9]|1[0-9]|2[0-9]|30)$"),
                    tidyselect::matches("^hm([1-9]|1[0-1])_name$"))),
      ~ as.character(.)
    ) %>%

    # Anonymise new household members
    dplyr::mutate(
      new_hm1_name =
        dplyr::if_else(!is.na(.data$new_hm1_name), "New HM1",
                       .data$new_hm1_name),
      new_hm2_name =
        dplyr::if_else(!is.na(.data$new_hm2_name), "New HM2",
                       .data$new_hm2_name),
      new_hm3_name =
        dplyr::if_else(!is.na(.data$new_hm3_name), "New HM3",
                       .data$new_hm3_name),
      new_hm4_name =
        dplyr::if_else(!is.na(.data$new_hm4_name), "New HM4",
                       .data$new_hm4_name)
    ) %>%

    # Anonymise contacts
    dplyr::mutate(
      c1 = dplyr::if_else(!is.na(.data$c1), "C1", .data$c1),
      c2 = dplyr::if_else(!is.na(.data$c2), "C2", .data$c2),
      c3 = dplyr::if_else(!is.na(.data$c3), "C3", .data$c3),
      c4 = dplyr::if_else(!is.na(.data$c4), "C4", .data$c4),
      c5 = dplyr::if_else(!is.na(.data$c5), "C5", .data$c5),
      c6 = dplyr::if_else(!is.na(.data$c6), "C6", .data$c6),
      c7 = dplyr::if_else(!is.na(.data$c7), "C7", .data$c7),
      c8 = dplyr::if_else(!is.na(.data$c8), "C8", .data$c8),
      c9 = dplyr::if_else(!is.na(.data$c9), "C9", .data$c9),
      c10 = dplyr::if_else(!is.na(.data$c10), "C10", .data$c10),
      c11 = dplyr::if_else(!is.na(.data$c11), "C11", .data$c11),
      c12 = dplyr::if_else(!is.na(.data$c12), "C12", .data$c12),
      c13 = dplyr::if_else(!is.na(.data$c13), "C13", .data$c13),
      c14 = dplyr::if_else(!is.na(.data$c14), "C14", .data$c14),
      c15 = dplyr::if_else(!is.na(.data$c15), "C15", .data$c15),
      c16 = dplyr::if_else(!is.na(.data$c16), "C16", .data$c16),
      c17 = dplyr::if_else(!is.na(.data$c17), "C17", .data$c17),
      c18 = dplyr::if_else(!is.na(.data$c18), "C18", .data$c18),
      c19 = dplyr::if_else(!is.na(.data$c19), "C19", .data$c19),
      c20 = dplyr::if_else(!is.na(.data$c20), "C20", .data$c20),
      c21 = dplyr::if_else(!is.na(.data$c21), "C21", .data$c21),
      c22 = dplyr::if_else(!is.na(.data$c22), "C22", .data$c22),
      c23 = dplyr::if_else(!is.na(.data$c23), "C23", .data$c23),
      c24 = dplyr::if_else(!is.na(.data$c24), "C24", .data$c24),
      c25 = dplyr::if_else(!is.na(.data$c25), "C25", .data$c25),
      c26 = dplyr::if_else(!is.na(.data$c26), "C26", .data$c26),
      c27 = dplyr::if_else(!is.na(.data$c27), "C27", .data$c27),
      c28 = dplyr::if_else(!is.na(.data$c28), "C28", .data$c28),
      c29 = dplyr::if_else(!is.na(.data$c29), "C29", .data$c29),
      c30 = dplyr::if_else(!is.na(.data$c30), "C30", .data$c30)
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
        dplyr::if_else(!is.na(.data$hm10_name), "HM10", .data$hm10_name),
      hm11_name =
        dplyr::if_else(!is.na(.data$hm11_name), "HM11", .data$hm11_name)
    )

}

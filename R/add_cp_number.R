#' @title Add CP Number
#'
#' @description Add CP Number to \code{data} by matching with email address.
#'
#' @param data Data frame including \code{email}.
#' @param reg_data Data frame including \code{email, cp_number}.
#' @param age_gender If \code{TRUE}, will also add age group and gender to
#' \code{data}.
#' @param age_wave Wave number of survey to calculate age at.
#' @param age_panel Panel of survey to calculate age at.
#' @param remove_email If \code{TRUE}, \code{email} will be removed from
#' returned data.
#'
#' @return \code{data} with \code{cp_number} (and \code{age_group} and
#' \code{gender} when \code{age_gender = TRUE}) added.
#'
#' @export

add_cp_number <- function(data,
                          reg_data,
                          age_gender = FALSE,
                          age_wave = NULL,
                          age_panel = NULL,
                          remove_email = FALSE) {

  # Check columns to add don't already exist in `data`

  if("cp_number" %in% names(data)){
    stop("`cp_number`` already in `data`.")
  }

  if(age_gender & any(c("age_group", "gender") %in% names(data))){
    stop("`age group` and/or `gender` already in `data`.")
  }

  # Check email exists in both datasets

  if(!"email" %in% names(data)){
    stop("`email` does not exist in `data`.")
  }

  if(!"email" %in% names(reg_data)){
    stop("`email` does not exist in `reg_data`.")
  }

  # Age and gender requirements

  if(age_gender & !all(c("date_of_birth", "gender") %in% names(reg_data))){
    stop("Both `date_of_birth` and `gender` must exist in `reg_data` when ",
         "`age_gender = TRUE`.")
  }

  if(age_gender & is.null(age_wave)){
    stop("If `age_gender = TRUE` then `wave` must be supplied.")
  }

  if(age_gender & is.null(age_panel)){
    if(age_wave < 44){
      stop("If `age_gender = TRUE` and wave is 43 or earlier, ",
           "`panel` must be supplied.")
    }
  }

  if(age_gender & !is.null(age_panel)){
    if(age_wave >= 44){
      age_panel <- NULL
      warning("Panels were merged from wave 44 onwards. ",
              "`panel` value supplied will not be used.")
    }
  }

  if(age_gender){
    lookup <- reg_data %>%
      dplyr::mutate(
        age_group =
          age(.data$date_of_birth, age_wave, age_panel, grouped = TRUE)) %>%
      dplyr::select(.data$email, .data$cp_number, .data$age_group, .data$gender)
  }else{
    lookup <- reg_data %>% dplyr::select(.data$email, .data$cp_number)
  }

  new_data <- data %>%
    dplyr::left_join(lookup, by = "email")

  if(any(is.na(new_data$cp_number))){
    stop(sum(is.na(new_data$cp_number)), " row(s) where CP Number not added. ",
         "Check all emails in `data` are present in `reg_data`.")
  }

  if(remove_email) new_data %<>% dplyr::select(-.data$email)

  return(new_data)

}

#' @title Update email address in registration data
#'
#' @param old_email Character string; email address to update.
#' @param new_email Character string; email address to replace \code{old_email}.
#' @param reg_data Data frame of registration data.
#'
#' @return Data frame of registration data with \code{old_email} replaced with
#' \code{new_email}.
#'
#' @export

update_email <- function(old_email, new_email, reg_data) {

  # Check email is in reg_data
  if(!"email" %in% names(reg_data)) {
    stop("`reg_data` does not have a variable named `email`.")
  }

  # Check old_email and new_email are length 1
  if(any(length(old_email) > 1, length(new_email) > 1)) {
    stop("`old_email` and `new_email` must be length 1.")
  }

  # Check old_email is in reg_data
  if(!old_email %in% reg_data$email) {
    warning("`old_email` supplied does not match any record in `reg_data`. \n",
            "`reg_data` will be returned unchanged.")
  }

  # Check new/old email are different
  if(old_email == new_email) {
    warning("`old_email` and `new_email` are the same. \n",
         "`reg_data` will be returned unchanged.")
  }

  reg_data %>%
    dplyr::mutate(email = ifelse(.data$email == old_email,
                                 new_email,
                                 .data$email))

}

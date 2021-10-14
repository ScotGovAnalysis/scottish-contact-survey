#' @title Delete non-anonymised data files
#'
#' @description To comply with data retention rules, non-anonymised data must
#' be regurlarly deleted. \code{delete_files} will delete any non-anonymised
#' data files in data folder for given \code{wave} and \code{panel}.
#'
#' @param wave Wave of survey data to delete
#' @param panel Panel of survey data to delete
#'
#' @export

delete_files <- function(wave, panel){

  # Get list of non-anonymised files
  to_delete <-
    list.files(
      here::here("data", paste0(wave, panel)),
      full.names = TRUE
    ) %>%

    magrittr::extract(!stringr::str_detect(., "-anon"))

  # Error if no files found
  if(length(to_delete) == 0){
    stop("No files found.")
  }

  # Print message listing all files to be deleted
  writeLines(
    paste0("Files to be deleted:\n",
           paste(to_delete, collapse = "\n"))
  )

  # Get user confirmation to proceed with deletion
  confirm <- readline(
    prompt = "Proceed to delete all files? (Y/N): "
  )

  # Error if user has entered anything other than 'Y'
  if(confirm != "Y"){
    stop("Files have not been deleted. ",
         "To delete, rerun and respond Y to confirm.")
  }

  # Delete files
  unlink(to_delete)
  print("Success: Files have been deleted.")

}
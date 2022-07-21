#' @title Delete non-anonymised data files
#'
#' @description To comply with data retention rules, non-anonymised data must
#' be regularly deleted. \code{delete_files} will delete any non-anonymised
#' data files in data folder for given \code{survey}.
#'
#' @param survey Character vector of surveys; e.g. 34A, 45. For waves 43 and
#' earlier, a panel letter (A or B) must be included.
#'
#' @export

delete_files <- function(survey){

  exists <- file.exists(here::here("data", survey))

  if(any(exists == FALSE)){
    stop("Following survey folders do not exist in data/: ",
         paste0(survey[!exists], collapse = ", "))
  }

  # Get list of non-anonymised files
  to_delete <-
    list.files(
      here::here("data", survey),
      full.names = TRUE
    ) %>%
    magrittr::extract(!stringr::str_detect(., "-anon") &
                        !stringr::str_detect(., "demographics-summary"))

  # Include registration file if exists
  to_delete <- c(
    to_delete,
    list.files(
      here::here("data", "registration-data"),
      pattern = paste(survey, collapse = "|"),
      full.names = TRUE
    )
  )

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
  message("Success: Files have been deleted.")

}
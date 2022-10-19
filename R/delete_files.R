#' @title Delete non-anonymised data files
#'
#' @description To comply with data retention rules, non-anonymised data must
#' be regularly deleted. \code{delete_files} will delete any non-anonymised
#' data files in \code{data_folder} for given \code{survey}.
#'
#' @param survey Character vector of surveys; e.g. 34A, 45. For waves 43 and
#' earlier, a panel letter (A or B) must be included.
#' @param data_folder Filepath of folder to search for data files.
#'
#' @export

delete_files <- function(survey,
                         data_folder = here::here("survey-data"),
                         user_confirm = TRUE){

  if(!file.exists(data_folder)){
    stop("`data_folder` does not exist.")
  }

  # Get list of non-anonymised files
  to_delete <-
    list.files(
      data_folder,
      pattern = as.character(survey),
      full.names = TRUE,
      recursive = TRUE
    ) %>%

    # Exclude any files including the word 'anon' or 'demographics-summary'
    magrittr::extract(!stringr::str_detect(., "-anon") &
                        !stringr::str_detect(., "demographics-summary"))

  # Error if no files found
  if(length(to_delete) == 0){
    stop("No files found.")
  }

  confirm <-
    if(user_confirm) {
      rstudioapi::showQuestion(
        title = "Delete files",
        message = paste0(
          "Are you sure you want to delete files?",
          "\n \n",
          paste(to_delete, collapse = "\n")
        ),
        ok = "Yes",
        cancel = "No"
      )
    } else {
      TRUE
    }

  # Error if user has responded 'No'
  if(!confirm){
    stop("Files have not been deleted. ",
         "To delete, rerun and click Yes to confirm.")
  }

  # Delete files
  if(confirm) {
    unlink(to_delete)
    message("Success: Files have been deleted.")
  }

}
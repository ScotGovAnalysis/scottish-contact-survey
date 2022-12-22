#' @title Backup data
#'
#' @description The function \code{backup_data} adds \code{file_to_backup} to
#' \code{zip_file}. If \code{zip_file} doesn't exist, it is created and
#' \code{file_to_backup} is added. If \code{zip_file} does exist,
#' \code{file_to_backup} is appended to existing zip file.
#'
#' @param zip_file Filepath to zip file; must end with .zip. If this file does
#' not already exist, it will be created.
#' @param file_to_backup Filepath to file to backup
#'
#' @export

backup_data <- function(zip_file, file_to_backup) {

  data_exists <- file.exists(file_to_backup)

  if(!data_exists) {
    stop("`file_to_backup` does not exist.")
  }

  if(!stringr::str_ends(zip_file, ".zip")) {
    stop("`zip_file` must be a zipped file, ending .zip")
  }

  zip_exists <- file.exists(zip_file)

  if(!zip_exists) {

    message("Creating ", zip_file)

    message("Backing up ", file_to_backup)

    zip::zipr(
      zipfile = zip_file,
      files = file_to_backup
    )

  }

  if(zip_exists) {

    message("Backing up ", file_to_backup)

    zip::zipr_append(
      zipfile = zip_file,
      files = file_to_backup
    )

  }

}

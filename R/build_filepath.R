#' @title Build filepath
#'
#' @description \code{build_filepath} returns a character vector of filepaths.
#'
#' @param filename Character vector containing file names.
#' @param survey Character string; one or two digits to denote survey wave
#' followed by an optional upper case A or B to denote panel; e.g. 2A, 30B, 50.
#' This is used to name the sub-directory of \code{data_folder} and to prefix
#' the file names.
#' @param fileext Character string; file extention to append to file name.
#' Default value is \code{.rds}. If supplied value doesn't begin with \code{.},
#' then this will be added.
#' @param data_folder Character string; full filepath of data folder. This
#' folder must already exist.
#' @param create_folder Logical; Default value of \code{TRUE} creates
#' sub-directory of \code{data_folder} named using \code{survey} (if it doesn't
#' already exist). If \code{FALSE}, the sub-folder is not created.
#'
#' @return A character vector of filepaths for the \code{filename}(s) provided.
#' The filepath begins at \code{data_folder}, followed by a sub-directory called
#' \code{survey}. Files are given a file extention determined by \code{fileext}.
#'
#' Note: The folder \code{data_folder/survey} will only be created if
#' \code{create_folder = TRUE}. The function does not check whether or not the
#' full filepaths returned exist.
#'
#' If \code{survey}, \code{fileext}, \code{data_folder} or \code{create_folder}
#' are more than length 1, then only the first value will be used.
#'
#' @export
#'
#' @examples
#' build_filepath(filename = c("file1", "file2"),
#'                survey = "50",
#'                data_folder = tempdir(),
#'                create_folder = FALSE)


build_filepath <- function(filename,
                            survey,
                            fileext = ".rds",
                            data_folder = here::here("survey-data"),
                            create_folder = TRUE) {

  # If more than one value supplied, use first only
  survey <- survey[1]
  fileext <- fileext[1]
  data_folder <- data_folder[1]
  create_folder <- create_folder[1]

  # Check survey in correct format
  if(!grepl("^\\d{1,2}[AB]{0,1}$", survey)) {
    stop("`survey` must be one or two digits followed by an optional upper ",
         "case A or B. e.g. 1A, 25B, or 50.")
  }

  # Check create_folder in correct format
  if(!inherits(create_folder, "logical")) {
    stop("`create_folder` must be logical; TRUE or FALSE.")
  }

  # Check data_folder exists
  if(!dir.exists(data_folder)) {
    stop("Folder does not exist: ", data_folder)
  }

  # Full filepath of survey subdirectory
  folder <- paste0(data_folder, "/", survey, "/")

  # Create subdirectory
  if(!dir.exists(folder) & create_folder) {
    dir.create(folder)
    if(dir.exists(folder)) message("Folder created: ", folder)
  }

  if(!dir.exists(folder) & !create_folder) {
    warning("Folder does not exist: ", folder, "\n",
            "To create folder, set `create_folder = FALSE`.")
  }

  # Prefix fileext with full stop if missing
  if(!grepl("^\\.", fileext)) fileext <- paste0(".", fileext)

  # Build full filepath for filenames
  paste0(folder, survey, "_", filename, fileext)

}

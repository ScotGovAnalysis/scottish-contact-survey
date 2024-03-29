#' @title Household Changes
#'
#' @description Get updates to household members from
#' non-anonymised response data for Scottish Contact Survey (SCS).
#'
#' @param resp_data Data frame of non-anonymised response data.
#' @param change_type Type of changes to be extracted; either "add" or "remove".
#'
#' @return Data frame of household changes.
#'
#' @export

household_changes <- function(resp_data, change_type = c("add", "remove")){

  # Check change_type valid
  if(!change_type %in% c("add", "remove")) {
    stop("`change_type` must be either 'add' or 'remove'.")
  }

  col_names <- if(change_type == "add") {
    purrr::pmap_chr(expand.grid(c("name", "age", "gender",
                                  "occupation", "student"),
                                1:4),
                    ~ paste0("new_hm", .y, "_", .x))
  } else {
    c(paste0("hm", 1:10, "_change"), paste0("hm", 1:10, "_name"))
  }

  col_names <- c("cp_number", col_names)

  # Check relevant columns exist in resp_data
  if(any(!col_names %in% names(resp_data))) {
    stop("One or more expected variables missing from `resp_data`.\n",
         "Missing variables: ",
         paste(setdiff(col_names, names(resp_data)),
               collapse = ", "))
  }

  changes <- resp_data %>%

    # Select rows with changes
    dplyr::filter(dplyr::if_any(
      tidyselect::matches(paste0("^hm_", change_type, "$")),
      ~ . == 1
    )) %>%

    # Keep required variables only
    dplyr::select(.data$cp_number, tidyselect::all_of(col_names))

  # Remove rows where no actual changes made

  if(change_type == "add") {

    changes %>%
      dplyr::filter(dplyr::if_any(
        tidyselect::matches("^new_hm\\d{1}_name$"),
        ~ !is.na(.)
      )) %>%

      dplyr::mutate(new_hm = purrr::reduce(
        dplyr::select(., tidyselect::matches("^new_hm[1-4]_name")) %>%
          dplyr::mutate(dplyr::across(tidyselect::everything(), ~ !is.na(.))),
        `+`
      ))

    changes %>%
      dplyr::rename_with(.cols = !.data$cp_number,
                         ~ stringr::str_remove(., "new_")) %>%
      tidyr::pivot_longer(cols = !.data$cp_number,
                          names_to = c("hm_add", ".value"),
                          names_sep = "_") %>%
      dplyr::filter(!is.na(.data$name))

  } else if(change_type == "remove") {

    changes %>%
      tidyr::pivot_longer(cols = !.data$cp_number,
                          names_to = c("hm_remove", ".value"),
                          names_sep = "_") %>%
      dplyr::filter(.data$change == 0 & !is.na(.data$name)) %>%
      dplyr::select(.data$cp_number, .data$hm_remove)

  }

}

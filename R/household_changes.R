#' @title Household Changes
#'
#' @description Get updates to household members from
#' non-anonymised response data for Scottish Contact Survey (SCS).
#'
#' @param resp_data Data frame of non-anonymised response data.
#' @param change_type Type of changes to be extracted; either "add" or "remove.
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

  # Check relevant columns exist in resp_data
  if(any(!col_names %in% c("cp_number", names(resp_data)))) {
    stop("One or more expected variables missing from `resp_data`.\n",
         "Missing variables: ",
         setdiff(col_names, c("cp_number", names(resp_data))))
  }

  changes <- resp_data %>%

    # Select rows with changes
    dplyr::filter_at(
      dplyr::vars(tidyselect::matches(paste0("hm_", change_type))),
      ~ . == 1
    ) %>%

    # Keep required variables only
    dplyr::select(cp_number, all_of(col_names))

  # Remove rows where no actual changes made

  if(change_type == "add") {

    changes %>%
      dplyr::filter_at(dplyr::vars(matches("^new_hm\\d{1}_name$")),
                       dplyr::any_vars(!is.na(.))) %>%

      dplyr::mutate(new_hm = reduce(
        dplyr::select(., tidyselect::matches("^new_hm[1-4]_name")) %>%
          dplyr::mutate_all(~ !is.na(.)), `+`))

    changes %>%
      dplyr::rename_at(vars(!cp_number), ~ str_remove(., "new_")) %>%
      tidyr::pivot_longer(cols = !cp_number,
                          names_to = c("hm_add", ".value"),
                          names_sep = "_") %>%
      filter(!is.na(name))

  } else if(change_type == "remove") {

    changes %>%
      tidyr::pivot_longer(cols = !cp_number,
                          names_to = c("hm_remove", ".value"),
                          names_sep = "_") %>%
      dplyr::filter(change == 0 & !is.na(name)) %>%
      dplyr::select(cp_number, hm_remove)

  }

}

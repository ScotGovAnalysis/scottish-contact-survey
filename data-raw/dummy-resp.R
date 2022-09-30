#########################################################################
# Name of file - dummy-resp.R
# Original Authors - Alice Byers
# Original Date - September 2022
#
# Description - Creates dummy response data for use in unit tests.
# Note: This script depends on resp_names and dummy_reg datasets.
#########################################################################


# Vaccine responses

vacc <- c("one dose",  paste(c("two", "three", "four", "five"), "doses"))
vacc_1_options <- c("No", paste("Yes, I have received", vacc))
vacc_2_options <- c("Yes, this is still correct", paste("I have now had", vacc))


# Household members to remove

hm_remove <- dummy_reg %>%
  dplyr::filter(status == "active") %>%
  dplyr::slice(1:50) %>%
  dplyr::mutate(
    hm_changes = sample(
      c("add", "remove", "both", "none"),
      dplyr::n(),
      replace = TRUE,
      prob = c(1, 1, 0.5, 2)
    ),
    hm_changes = dplyr::if_else(n_household == 1, NA_character_, hm_changes)
  ) %>%
  dplyr::select(cp_number,
                hm_changes,
                tidyselect::matches("^hm\\d{1,2}_name$")) %>%
  tidyr::pivot_longer(cols = !c(cp_number, hm_changes),
                      names_to = "col_name", values_to = "name") %>%
  dplyr::mutate(
    col_name = stringr::str_remove(col_name, "_name"),
    change = sample(0:1, dplyr::n(), replace = TRUE),
    change =
      dplyr::if_else(!hm_changes %in% c("remove", "both") | is.na(name),
                     NA_integer_,
                     change)
  ) %>%
  tidyr::pivot_wider(names_from = col_name,
                     names_glue = "{col_name}_{.value}",
                     values_from = c(name, change))


# New household members

cp_new_hm <-
  hm_remove %>%
  dplyr::filter(hm_changes %in% c("add", "both")) %>%
  dplyr::pull(cp_number)

hm_new <-
  tibble::tibble(
    cp_number = rep(
      cp_new_hm,
      times = sample(1:4, length(cp_new_hm), replace = TRUE, prob = 4:1)
    )
  ) %>%
  dplyr::group_by(cp_number) %>%
  dplyr::mutate(col_name = paste0("new_hm", dplyr::row_number()),
                name = paste0("non-anon new_hm", dplyr::row_number()),
                age = "dummy_age",
                gender = "dummy_gender",
                occupation = "dummy_occupation",
                student = "dummy_student") %>%
  dplyr::ungroup() %>%
  tidyr::pivot_wider(names_from = col_name,
                     names_glue = "{col_name}_{.value}",
                     values_from = c(name, age, gender, occupation, student))


# All household changes

hm_changes <- dplyr::full_join(hm_remove, hm_new, by = "cp_number")


dummy_resp <-

  matrix(nrow = 50, ncol = nrow(resp_names)) %>%
  tibble::as_tibble(.name_repair = ~ resp_names$names) %>%

  # Add columns containing household member changes
  dplyr::select(-tidyselect::any_of(names(hm_changes))) %>%
  dplyr::bind_cols(hm_changes) %>%
  dplyr::mutate(
    hm_remove = dplyr::case_when(
      hm_changes %in% c("remove", "both") ~ 1,
      hm_changes == "none" | is.na(hm_changes) ~ NA_real_,
      hm_changes == "add" ~ 0
    ),
    hm_add = dplyr::case_when(
      hm_changes %in% c("add", "both") ~ 1,
      hm_changes == "none" | is.na(hm_changes) ~ NA_real_,
      hm_changes == "remove" ~ 0
    ),
    hm_changes = dplyr::case_when(
      hm_changes %in% c("remove", "add", "both") ~
        "No, this is incorrect or there have been changes to my household",
      hm_changes == "none" | is.na(hm_changes) ~ "Yes, this is correct"
    )
  ) %>%

  # Add contact names
  dplyr::mutate(
    dplyr::across(
      tidyselect::matches("^c\\d{1,2}$"),
      ~ sample(c(paste("non-anon", dplyr::cur_column()), NA),
               dplyr::n(),
               replace = TRUE)
    )
  ) %>%

  # Add Email
  dplyr::mutate(email = paste0(cp_number, "@email.com")) %>%

  # Add vaccine data
  dplyr::mutate(
    vacc_1 = c(rep(NA_character_, times = 25),
               sample(vacc_1_options, 25, replace = TRUE)),
    vacc_2 = c(sample(vacc_2_options, 25, replace = TRUE),
               rep(NA_character_, times = 25))
  ) %>%

  # Order columns as in resp_names
  dplyr::select(cp_number, tidyselect::all_of(resp_names$names))


# Save to data/
usethis::use_data(dummy_resp, overwrite = TRUE)


### END OF SCRIPT ###
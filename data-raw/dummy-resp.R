#########################################################################
# Name of file - dummy-resp.R
# Original Authors - Alice Byers
# Original Date - September 2022
#
# Description - Creates dummy response data for use in unit tests.
# NOTE: This dataset does not contain all variables expected in the
# actual response data, but just those required for unit tests. For full
# list of variables in actual data, see `scs::resp_names`.
#########################################################################


hm_contact_names <- c("cp_number", "email",
                      paste0("hm", 1:10, "_name"),
                      paste0("new_hm", 1:4, "_name"),
                      paste0("c", 1:30))

vacc <- c("one dose",  paste(c("two", "three", "four", "five"), "doses"))

vacc_1_options <- c("No", paste("Yes, I have received", vacc))
vacc_2_options <- c("Yes, this is still correct", paste("I have now had", vacc))


dummy_resp <-

  # Add columns containing household member and contact name/nicknames
  matrix(nrow = 50, ncol = length(hm_contact_names)) %>%
  tibble::as_tibble(.name_repair = ~ hm_contact_names) %>%
  dplyr::mutate(
    dplyr::across(
      tidyselect::all_of(hm_contact_names),
      ~ sample(c(paste("non-anon", dplyr::cur_column()), NA),
               dplyr::n(),
               replace = TRUE)
    )
  ) %>%

  # Add CP Number and Email
  dplyr::mutate(
    cp_number = paste0(
      "CP", c("A", "B", "Z"),
      stringr::str_pad(dplyr::row_number(), width = 4, pad = "0", side = "left")
    ),
    email = paste0(cp_number, "@email.com"),
    .before = everything()
  ) %>%

  # Add vaccine data
  dplyr::mutate(
    vacc_1 = c(rep(NA_character_, times = 25),
               sample(vacc_1_options, 25, replace = TRUE)),
    vacc_2 = c(sample(vacc_2_options, 25, replace = TRUE),
               rep(NA_character_, times = 25))
  )


# Save to data/
usethis::use_data(dummy_resp, overwrite = TRUE)


### END OF SCRIPT ###
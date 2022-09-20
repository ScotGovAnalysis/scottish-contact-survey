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


names <- c("email",
           paste0("hm", 1:10, "_name"),
           paste0("new_hm", 1:4, "_name"),
           paste0("c", 1:30))

dummy_resp <-
  matrix(nrow = 50, ncol = length(names)) %>%
  tibble::as_tibble(.name_repair = ~ names) %>%
  dplyr::mutate(
    email = paste0(
      "CP", c("A", "B", "Z"),
      stringr::str_pad(dplyr::row_number(), width = 4, pad = "0", side = "left"),
      "@email.com"
    ),
    dplyr::across(
      !email,
      ~ sample(c(paste("non-anon", dplyr::cur_column()), NA),
               dplyr::n(),
               replace = TRUE)
    )
  )

# Save to data/
usethis::use_data(dummy_resp, overwrite = TRUE)


### END OF SCRIPT ###
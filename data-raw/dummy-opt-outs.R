#########################################################################
# Name of file - dummy-opt-outs.R
# Original Authors - Alice Byers
# Original Date - September 2022
#
# Description - Creates dummy opt out data for use in unit tests.
#########################################################################


dummy_opt_outs <-
  tidyr::expand_grid(
    age_group = c("18-29", "30-39", "40-49", "50-59", "60-69", "70+"),
    gender = c("Female", "Male", "Non-Binary", "In another way")
  ) %>%
  dplyr::mutate(n_opt_outs = sample(1:4, size = nrow(.), replace = TRUE)) %>%
  dplyr::sample_n(10)


# Save to data/
usethis::use_data(dummy_opt_outs, overwrite = TRUE)


### END OF SCRIPT ###
#########################################################################
# Name of file - dummy-reg.R
# Original Authors - Alice Byers
# Original Date - September 2022
#
# Description - Creates dummy registration data for use in unit tests.
#########################################################################


# Registration data variable names
names <- c(
  "cp_number", "status", "panel", "email", "date_of_birth", "gender",
  "postcode", "local_authority_code", "local_authority", "ethnicity",
  "other_ethnicity", "other_hm", "employment", "studying",
  "highest_earner_flag", "occupation_1", "occupation_2",
  "highest_earner_occupation_1", "highest_earner_occupation_2",
  "total_household_income", "vaccine_n_doses", "last_updated"
)

# Create household member data for 75 participants
hm <-
  tibble::tibble(
    id = rep(1:75, times = sample(0:10, 75, replace = TRUE))
  ) %>%
  dplyr::group_by(id) %>%
  dplyr::mutate(hm = paste0("hm", dplyr::row_number()),
                n_household = dplyr::n() + 1) %>%
  dplyr::ungroup() %>%
  dplyr::mutate(name = "dummy_name",
                age = "dummy_age",
                gender = "dummy_gender",
                occupation = "dummy_occupation",
                student = "dummy_student") %>%
  tidyr::pivot_wider(names_from = hm, values_from = name:student,
                     names_glue = "{hm}_{.value}") %>%
  tidyr::complete(id = 1:75, fill = list(n_household = 1)) %>%
  dplyr::select(
    n_household,
    purrr::pmap_chr(
      expand.grid(c("name", "age", "gender", "occupation", "student"), 1:10),
      ~ paste0("hm", .y, "_", .x)
    )
  )

dummy_reg <-

  # Create empty tibble
  matrix(nrow = 50, ncol = length(names)) %>%
  tibble::as_tibble(.name_repair = ~ names) %>%

  # Add rows for active panel members
  dplyr::mutate(
    cp_number = paste0(
      "CP", c("A", "B", "Z"),
      stringr::str_pad(dplyr::row_number(), width = 4, pad = "0", side = "left")
    ),
    status = "active",
    panel = stringr::str_sub(cp_number, 3, 3)
  ) %>%

  # Add rows for reserves
  tibble::add_row(status = rep("reserve", times = 25),
                  panel = rep("reserve", times = 25)) %>%

  # Add registration data
  dplyr::mutate(
    email = dplyr::if_else(
      status == "active",
      paste0(cp_number, "@email.com"),
      paste0("reserve", dplyr::row_number(), "@email.com")
    ),
    date_of_birth =
      sample(
        seq(lubridate::dmy(01011950), lubridate::dmy(31122000), by = "day"),
        dplyr::n()
      ),
    gender =
      sample(c("Female", "Male", "Non-Binary", "In another way"),
             dplyr::n(),
             replace = TRUE),
    dplyr::across(postcode:vaccine_n_doses, ~ "dummy_value"),
    last_updated = lubridate::dmy(01092022)
  ) %>%

  # Add household members
  dplyr::bind_cols(hm) %>%
  dplyr::relocate(n_household, .after = "other_ethnicity") %>%

  # Add rows for opt-outs
  tibble::add_row(
    cp_number = paste0("CP", c("A", "B", "Z"), "00", 75:99),
    status = rep("opt-out", times = 25),
    panel = rep_len(c("A", "B", "Z"), length.out = 25)
  )


# Save to data/
usethis::use_data(dummy_reg, overwrite = TRUE)


### END OF SCRIPT ###
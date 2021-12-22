#########################################################################
# Name of file - 03_prep-next-wave.R
# Data release - Weekly Scottish Contact Survey Analysis
# Original Authors - Alice Byers
# Original Date - August 2021
#
# Type - Reproducible Analytical Pipeline (RAP)
# Written/run on - RStudio Desktop
# Version of R - 3.6.3
#
# Description - Replace opt outs from reserve list and get information
# for Questback invites for next wave.
#########################################################################


### 0 - Setup ----

source(here::here("code", "00_setup.R"))


### 1 - Get registration data ----

reg <-
  here("data", "registration-data",
       paste0(cur_survey, "_registration-data.rds")) %>%
  read_rds()


### 2 - Replace opt outs ----

# # Get data for people on reserve list
# reserve_data <-
#   reg %>%
#   mutate(age_group = age_group(age(date_of_birth, cur_wave, cur_panel))) %>%
#   filter(status == "reserve") %>%
#   select(email, age_group, gender)
#
# # Get data for opt outs
# opt_out_data <-
#   here("data", cur_survey, paste0(cur_survey, "_opt-outs-anon.rds")) %>%
#   read_rds() %>%
#   count(age_group, gender) %>%
#   rename(n_opt_outs = n)
#
# # Get list of emails and new cp numbers for replacement
# replace <-
#   tibble(
#     email = replace_opt_outs(reserve_data, opt_out_data)
#   ) %>%
#   mutate(new_cp = generate_cp_number(reg$cp_number, cur_panel, n = nrow(.)))
#
# # Update registration data to add replacements to current panel
# reg %<>%
#   left_join(replace, by = "email") %>%
#   mutate(
#     panel = if_else(!is.na(new_cp), cur_panel, panel),
#     status = if_else(!is.na(new_cp), "active", status),
#     cp_number = if_else(!is.na(new_cp), new_cp, cp_number)
#   ) %>%
#   select(-new_cp)


### 3 - Save updated registration data ----

write_rds(
  reg,
  here("data", "registration-data",
       paste0(next_wave, next_panel, "_registration-data.rds")),
  compress = "gz"
)


### 4 - Get data required for next wave QB invites ----

invites <-
  reg %>%
  filter(status == "active" & panel == cur_panel) %>%

  # Add flag for reg data to be updated
  mutate(to_update = ifelse(is.na(last_updated), 1, 0)) %>%

  select(email, contains("_name"), employment_status, studying_location,
         vaccine_n_doses, to_update) %>%

  # Rename columns for Questback
  set_names(c("email", paste0("HM", 1:10), "Employment status (raw)",
              "Studying", "Vaccine", "to_update"))

write_csv(
  invites,
  here("data", paste0(cur_wave + 1, cur_panel),
       paste0(cur_wave + 1, cur_panel, "_qb-invites.csv")),
  na = ""
)


### 5 - Delete non-anonymised data files ----

# Keep rolling four-week history of non-anonymised files
# Delete non-anon files for two waves prior to current survey

delete_files(cur_wave - 2, cur_panel)


### END OF SCRIPT ###
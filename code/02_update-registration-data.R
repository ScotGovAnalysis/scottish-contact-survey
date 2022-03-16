#########################################################################
# Name of file - 02_update-registration-data.R
# Data release - Weekly Scottish Contact Survey Analysis
# Original Authors - Alice Byers
# Original Date - July 2021
#
# Type - Reproducible Analytical Pipeline (RAP)
# Written/run on - RStudio Desktop
# Version of R - 3.6.3
#
# Description - Remove personal data for opt outs and update household
# information in registration data.
#########################################################################


### 0 - Setup ----

source(here::here("code", "00_setup.R"))


### 1 - Get data ----

reg <-
  here("data", "registration-data",
       paste0(cur_survey, "_registration-data.rds")) %>%
  read_rds()


### 2 - Recode opt outs ----

# For opt outs, keep CP number and remove all other registration data

reg %<>%
  recode_opt_outs(
    here("data", cur_survey, paste0(cur_survey, "_opt-outs-anon.rds")) %>%
      read_rds() %>%
      pull(cp_number)
  )


### 3 - Update registration data with household member changes ----

remove <- here("data", cur_survey, paste0(cur_survey, "_hm-removed.rds")) %>%
  read_rds()

add <- here("data", cur_survey, paste0(cur_survey, "_hm-added.rds")) %>%
  read_rds()

reg %<>% update_household_members(remove, add)


### 7 - Update vaccination data ----

reg %<>%
  left_join(vaccine_changes(cur_wave, cur_panel), by = "cp_number") %>%
  mutate(
    vaccine_n_doses = case_when(
      !is.na(vaccine_n_doses_new) ~ vaccine_n_doses_new,
      TRUE ~ vaccine_n_doses
    )
  ) %>%
  select(-vaccine_n_doses_new)


### 8 - Update registration data where refreshed in latest survey ----

updates <- reg_data_updates(cur_wave, cur_panel)

reg %<>%

  # People without updated info
  filter(!cp_number %in% updates$cp_number) %>%

  # People with updated info
  bind_rows(
    reg %>%
      filter(cp_number %in% updates$cp_number) %>%
      select(-all_of(setdiff(names(updates), "cp_number"))) %>%
      left_join(updates, by = "cp_number")
  )


### 9 - Save updated registration data ----

write_rds(
  new_reg,
  here("data", "registration-data",
       paste0(cur_survey, "_registration-data.rds")),
  compress = "gz"
)


### 10 - Save anonymised registration data for current wave ----

# CP numbers for survey responses
cp_responses <-
  here("data", cur_survey, paste0(cur_survey, "_response-data-anon.rds")) %>%
  read_rds() %>%
  pull(cp_number)

anon_reg <-
  reg %>%
  filter(cp_number %in% cp_responses) %>%
  select(-email) %>%
  anon_reg_data()

write_rds(
  anon_reg,
  here("data", cur_survey, paste0(cur_survey, "_registration-data-anon.rds")),
  compress = "gz"
)

# Save backup
write_rds(
  anon_reg,
  paste0("//s0177a/datashare/CoMix/Private/CoMix Model/Backup Data/",
         cur_survey, "_registration-data-anon.rds"),
  compress = "gz"
)

# Temp - reformat as required for controller script
# Future work will incorporate controllor script into SCS package
# This section (and associated functions) can be dropped once this is done.

temp_anon_reg <-
  reformat_anon_reg(anon_reg,
                    read_rds(here("lookups", "anon-sample-names.rds"))$names,
                    cur_wave,
                    cur_panel) %T>%
  write_csv(
    here("data", cur_survey, paste0(cur_survey, "_registration-data-anon.csv"))
  )


### END OF SCRIPT ###
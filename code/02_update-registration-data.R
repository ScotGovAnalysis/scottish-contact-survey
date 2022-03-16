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

new_reg %<>%
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

new_reg %<>%

  # People without updated info
  filter(!cp_number %in% updates$cp_number) %>%

  # People with updated info
  bind_rows(
    new_reg %>%
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
  new_reg %>%
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
# Future work will incorporate controllor script into comix package
# This section can be dropped once this is done.

temp_anon_reg <- anon_reg %>%

  # Temp - add missing columns and reorder
  select(-status, -panel, -local_authority_code,
         -contains("vaccine"), -last_updated) %>%
  add_column(occupation_3 = NA, .after = "occupation_2") %>%
  add_column(high_risk = NA, medium_risk = NA, .after = "other_ethnicity") %>%
  add_column(hh_changes = NA, .after = "cp_number") %>%
  add_column(ethnicity2 = NA, .after = "ethnicity") %>%
  add_column(studying_yn = NA, .after = "employment") %>%
  add_column(also_employed = NA, furloughed = NA, .after = "studying") %>%

  select(cp_number:n_household,
         matches("hm\\d{1,2}_name"), everything()) %>%
  add_column(
    `12.1` = NA, `12.2` = NA, `12.3` = NA,
    `12.4` = NA, `12.5` = NA, `12.6` = NA,
    `12.7` = NA, `12.8` = NA, `12.9` = NA,
    `12.10` = NA, `12.11` = NA, `12.12` = NA,
    .after = "hm10_name"
  ) %>%
  select(cp_number:`12.12`,
         employment:total_household_income,
         everything()) %>%
  mutate(date_of_birth = age(date_of_birth, cur_wave, cur_panel)) %>%

  # Temp - rename variables for controller script
  set_names(read_rds(here("lookups", "anon-sample-names.rds"))$names)

write_csv(
  temp_anon_reg,
  here("data", cur_survey, paste0(cur_survey, "_registration-data-anon.csv"))
)


### END OF SCRIPT ###
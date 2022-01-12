#########################################################################
# Name of file - 01_process-response-data.R
# Data release - Weekly Scottish Contact Survey Analysis
# Original Authors - Alice Byers
# Original Date - July 2021
#
# Type - Reproducible Analytical Pipeline (RAP)
# Written/run on - RStudio Desktop
# Version of R - 3.6.3
#
# Description - Process response data as downloaded from Questback; clean
# data, add CP number, get household changes, anonymise. Save file with
# cp numbers for opt-outs.
#########################################################################


### 0 - Setup ----

source(here::here("code", "00_setup.R"))


### 1 - Clean response data file ----

resp <-

  # Read in raw response data
  here("data", cur_survey,
       paste0(cur_survey, "_response-data.xlsx")) %>%
  read.xlsx(sheet = "Raw Data") %>%

  # Clean names
  set_names(read_rds(here("lookups", "response-data-names.rds"))$new_names) %>%

  # Fix formatting
  mutate(date_completed = janitor::excel_numeric_to_date(date_completed)) %>%
  mutate_at(vars(matches("^new_hm[1-4]")), ~ as.character(.)) %>%

  # Remove responses where consent not given
  filter(!is.na(consent))


# Add CP Number

cp_number_lookup <-
  here("data", "registration-data",
       paste0(cur_survey, "_registration-data.rds")) %>%
  read_rds() %>%
  select(cp_number, email, date_of_birth, gender)

resp %<>%
  left_join(cp_number_lookup %>% select(cp_number, email), by = "email") %>%
  select(cp_number, everything())


### 2 - Run prize draw ----

winner <-
  resp %>%
  select(cp_number, email) %>%
  sample_n(1)

write_csv(
  winner,
  here("data", cur_survey, paste0(cur_survey, "_prize-draw.csv"))
)


### 3 - Get household and vaccine changes ----

hm_changes <-
  resp %>%
  filter(str_starts(hm_changes, "No")) %>%
  select(cp_number, hm_remove:new_hm4_student)

write_rds(
  hm_changes,
  here("data", cur_survey, paste0(cur_survey, "_hm-changes.rds")),
  compress = "gz"
)

vacc_changes <-
  resp %>%
  filter(!is.na(vacc_1) |
           (!is.na(vacc_2) & vacc_2 != "Yes, this is still correct.")) %>%
  mutate_at(
    vars(c(vacc_1, vacc_2)),
    ~ case_when(
      str_detect(., "booster") ~ "three doses",
      str_detect(., "two doses") ~ "two doses",
      str_detect(., "one dose") ~ "one dose",
      . == "No" ~ "no doses",
      TRUE ~ NA_character_
    )) %>%
  mutate(vaccine_n_doses_new = ifelse(!is.na(vacc_1), vacc_1, vacc_2)) %>%
  select(cp_number, vaccine_n_doses_new)

write_rds(
  vacc_changes,
  here("data", cur_survey, paste0(cur_survey, "_vacc-changes.rds")),
  compress = "gz"
)


### 4 - Anonymise response data ----

anon_resp <-
  resp %>%
  select(-email) %>%
  anon_response_data()

write_rds(
  anon_resp,
  here("data", cur_survey, paste0(cur_survey, "_response-data-anon.rds")),
  compress = "gz"
)

# Save backup
write_rds(
  anon_resp,
  paste0("//s0177a/datashare/CoMix/Private/CoMix Model/Backup Data/",
         cur_survey, "_response-data-anon.rds"),
  compress = "gz"
)

# Temp - reformat as required for controller script
# Future work will incorporate controllor script into comix package
# This section can be dropped once this is done.

temp_anon_resp <- anon_resp %>%

  # Temp - remove some columns
  select(-vacc_1, -vacc_2, -vacc_3, -lateral_flow,
         -in_scotland, -vaccine, -panel, -to_update, -matches("^updated_")) %>%

  # Temp - move some columns to end for controller script
  select(setdiff(names(.),
                 c("old_vaccination", "old_n_vacc_doses",
                   "sheilding", "long_term_condition")),
         old_vaccination, old_n_vacc_doses, sheilding, long_term_condition) %>%

  # Temp - add empty columns
  add_column(hm11_name = NA, employment_1_0 = NA, .after = 1700) %>%
  add_column(child1 = NA, child2 = NA, .after = 1703) %>%
  magrittr::inset(sprintf("test_%d", 1:52), value = NA) %>%

  # Temp - move some columns to end for controller script
  select(setdiff(names(.),
                 c("old_vaccination", "old_n_vacc_doses",
                   "sheilding", "long_term_condition")),
         old_vaccination, old_n_vacc_doses, sheilding, long_term_condition) %>%

  # Temp - rename variables for controller script
  set_names(read_rds(here("lookups", "anon-response-names.rds"))$names)

write_csv(
  temp_anon_resp,
  here("data", cur_survey, paste0(cur_survey, "_response-data-anon.csv"))
)


### 5 - Get opt outs ----

opt_outs <-
  here("data", cur_survey, paste0(cur_survey, "_opt-outs.xlsx")) %>%
  read.xlsx(sheet = 1) %>%
  select(email = `E-mail`) %>%
  left_join(cp_number_lookup, by = "email") %>%
  mutate(age_group = age_group(age(date_of_birth, cur_wave, cur_panel))) %>%
  select(-email, -date_of_birth)

# Save list of cp number, age and gender only
write_rds(
  opt_outs,
  here("data", cur_survey, paste0(cur_survey, "_opt-outs-anon.rds")),
  compress = "gz"
)


### END OF SCRIPT ###
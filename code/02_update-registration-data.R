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
# Description - Replace opt-outs, update registration data, save
# anonymised registration data for survey respondents, save invite data
# for next survey and delete old non-anon files.
#########################################################################


### 0 - Setup ----

source(here::here("code", "00_setup.R"))


### 1 - Get data ----

reg <-
  scs_filepath("registration-data", pre_wave, registration = TRUE) %>%
  read_rds()

resp <-
  scs_filepath("response-data-anon", wave) %>%
  read_rds()


### 2 - Recode and replace opt outs ----

opt_outs <-
  scs_filepath("opt-outs-anon", wave) %>%
  read_rds()

# Remove opt outs from registration data
reg %<>% filter(!cp_number %in% opt_outs$cp_number)

# Replace opt-outs from reserve list
if(add_reserves == TRUE) {
  reg %<>%
    replace_opt_outs(
      opt_outs %>% count(age_group, gender) %>% rename(n_opt_outs = n),
      wave
    )
}


### 3 - Update registration data ----

# Changes to household members

remove <- scs_filepath("hm-removed", wave) %>% read_rds()

add <- scs_filepath("hm-added", wave) %>% read_rds()

reg %<>% update_household_members(remove, add)


# Changes to vaccine status

reg %<>% vaccine_changes(resp %>% select(cp_number, vacc_1, vacc_2))


# Changes to other registration data

reg %<>% reg_data_updates(resp, start_date(wave))


### 4 - Save anonymised registration data for current wave ----

anon_reg <-
  reg %>%
  filter(cp_number %in% resp$cp_number) %>%
  select(-email) %>%
  anonymise_data("reg")

write_rds(
  anon_reg,
  scs_filepath("registration-data-anon", wave),
  compress = "gz"
)

# Save backup
backup_data(
  zip_file = "//s0177a/datashare/CoMix/Private/CoMix Model/Backup Data.zip",
  file_to_backup = scs_filepath("registration-data-anon", wave)
)

# Temp - reformat data as required for controller script
# Future work will incorporate controllor script into this RAP
# This section (and associated functions) can be dropped once this is done.

temp_anon_reg <-
  reformat_anon_reg(anon_reg,
                    read_rds(here("lookups", "anon-sample-names.rds"))$names,
                    wave)

write_csv(
  temp_anon_reg,
  scs_filepath("registration-data-anon", wave, fileext = ".csv")
)


### 5 - Save updated registration data ----

write_rds(
  reg,
  scs_filepath("registration-data", wave, registration = TRUE),
  compress = "gz"
)


### 6 - Get invite data for next wave of survey ----

invites <- survey_invites(reg)

write_csv(
  invites,
  scs_filepath("qb-invites", next_wave, fileext = ".csv"),
  na = ""
)


### 7 - Delete non-anonymised data files ----

# Keep rolling four-week history of non-anonymised files
# Delete non-anon files for four waves prior to current survey

delete_files(wave - 4)


### 8 - Produce demographics summary report ----

render(
  input = here("markdown", "demographics-summary.Rmd"),
  output_file = scs_filepath("demographics-summary", wave, fileext = ".html")
)


### END OF SCRIPT ###
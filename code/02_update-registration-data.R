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
  here("data", "registration-data",
       paste0(pre_wave, "_registration-data.rds")) %>%
  read_rds()

resp <-
  here("data", wave, paste0(wave, "_response-data-anon.rds")) %>%
  read_rds()


### 2 - Recode and replace opt outs ----

opt_outs <-
  here("data", wave, paste0(wave, "_opt-outs-anon.rds")) %>%
  read_rds()

# Remove personal data for opt-outs
reg %<>% remove_opt_outs(opt_outs$cp_number)

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

remove <- here("data", wave, paste0(wave, "_hm-removed.rds")) %>%
  read_rds()

add <- here("data", wave, paste0(wave, "_hm-added.rds")) %>%
  read_rds()

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
  here("data", wave, paste0(wave, "_registration-data-anon.rds")),
  compress = "gz"
)

# Save backup
write_rds(
  anon_reg,
  paste0("//s0177a/datashare/CoMix/Private/CoMix Model/Backup Data/",
         wave, "_registration-data-anon.rds"),
  compress = "gz"
)

# Temp - reformat as required for controller script
# Future work will incorporate controllor script into SCS package
# This section (and associated functions) can be dropped once this is done.

temp_anon_reg <-
  reformat_anon_reg(anon_reg,
                    read_rds(here("lookups", "anon-sample-names.rds"))$names,
                    wave)

write_csv(
  temp_anon_reg,
  here("data", wave, paste0(wave, "_registration-data-anon.csv"))
)


### 5 - Save updated registration data ----

write_rds(
  reg,
  here("data", "registration-data",
       paste0(wave, "_registration-data.rds")),
  compress = "gz"
)


### 6 - Get invite data for next wave of survey ----

invites <- survey_invites(reg)

write_csv(
  invites,
  here("data", wave + 1, paste0(wave + 1, "_qb-invites.csv")),
  na = ""
)


### 7 - Delete non-anonymised data files ----

# Keep rolling four-week history of non-anonymised files
# Delete non-anon files for four waves prior to current survey

delete_files(wave - 4)


### 8 - Produce demographics summary report ----

render(
  input = here("markdown", "demographics-summary.Rmd"),
  output_file = here("data", wave, paste0(wave, "_demographics-summary.html"))
)


### END OF SCRIPT ###
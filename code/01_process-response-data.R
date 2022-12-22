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
  scs_filepath("response-data", wave, fileext = ".csv") %>%
  read_csv(col_types = paste(resp_names$type, collapse = "")) %>%

  # Clean names
  set_names(resp_names$names) %>%

  # Format date
  mutate(date_completed = dmy_hms(date_completed)) %>%

  # Remove responses where consent not given or not in scotland
  filter(!is.na(consent) & in_scotland == "Yes") %>%

  # Add CP Number
  add_cp_number(
    scs_filepath("registration-data", pre_wave, registration = TRUE) %>%
      read_rds()
  ) %>%
  select(cp_number, everything())


### 2 - Run prize draw ----

winner <-
  resp %>%
  select(cp_number, email) %>%
  sample_n(1)

write_csv(
  winner,
  scs_filepath("prize-draw", wave, fileext = ".csv")
)


### 3 - Get household changes ----

hm_removed <-
  household_changes(resp, "remove")

write_rds(
  hm_removed,
  scs_filepath("hm-removed", wave),
  compress = "gz"
)

hm_added <-
  household_changes(resp, "add")

write_rds(
  hm_added,
  scs_filepath("hm-added", wave),
  compress = "gz"
)


### 4 - Anonymise response data ----

anon_resp <-
  resp %>%
  select(-email) %>%
  anonymise_data("resp")

write_rds(
  anon_resp,
  scs_filepath("response-data-anon", wave),
  compress = "gz"
)

# Save backup
backup_data(
  zip_file = "//s0177a/datashare/CoMix/Private/CoMix Model/Backup Data.zip",
  file_to_backup = scs_filepath("response-data-anon", wave)
)

# Temp - reformat data as required for controller script
# Future work will incorporate controllor script into this RAP
# This section (and associated functions) can be dropped once this is done.

temp_anon_resp <-
  reformat_anon_resp(
    anon_resp,
    read_rds(here("lookups", "anon-response-names.rds"))$names
  )

write_csv(
  temp_anon_resp,
  scs_filepath("response-data-anon", wave, fileext = ".csv")
)


### 5 - Get opt outs ----

opt_outs <-
  scs_filepath("opt-outs", wave, fileext = ".xlsx") %>%
  read_xlsx(sheet = 1) %>%
  select(email = `E-mail`) %>%
  add_cp_number(
    scs_filepath("registration-data", pre_wave, registration = TRUE) %>%
      read_rds(),
    age_gender = TRUE,
    age_wave = wave,
    remove_email = TRUE
  )

# Save list of cp number, age and gender only
write_rds(
  opt_outs,
  scs_filepath("opt-outs-anon", wave),
  compress = "gz"
)


### END OF SCRIPT ###
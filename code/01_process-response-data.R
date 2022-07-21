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

resp_names <- read_rds(here("lookups", "response-data-names.rds"))

resp <-

  # Read in raw response data
  here("data", wave, paste0(wave, "_response-data.csv")) %>%
  read_csv(col_types = paste(resp_names$type, collapse = "")) %>%

  # Clean names
  set_names(resp_names$new_names) %>%

  # Format date
  mutate(date_completed = dmy_hms(date_completed)) %>%

  # Remove responses where consent not given or not in scotland
  filter(!is.na(consent) & in_scotland == "Yes")


# Add CP Number

resp %<>%
  add_cp_number(
    here("data", "registration-data",
         paste0(pre_wave, "_registration-data.rds")) %>%
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
  here("data", wave, paste0(wave, "_prize-draw.csv"))
)


### 3 - Get household changes ----

hm_removed <-
  household_changes(resp, "remove")

write_rds(
  hm_removed,
  here("data", wave, paste0(wave, "_hm-removed.rds")),
  compress = "gz"
)

hm_added <-
  household_changes(resp, "add")

write_rds(
  hm_added,
  here("data", wave, paste0(wave, "_hm-added.rds")),
  compress = "gz"
)


### 4 - Anonymise response data ----

anon_resp <-
  resp %>%
  select(-email) %>%
  anon_response_data()

write_rds(
  anon_resp,
  here("data", wave, paste0(wave, "_response-data-anon.rds")),
  compress = "gz"
)

# Save backup
write_rds(
  anon_resp,
  paste0("//s0177a/datashare/CoMix/Private/CoMix Model/Backup Data/",
         wave, "_response-data-anon.rds"),
  compress = "gz"
)

# Temp - reformat as required for controller script
# Future work will incorporate controllor script into comix package
# This section can be dropped once this is done.

temp_anon_resp <-
  reformat_anon_resp(
    anon_resp,
    read_rds(here("lookups", "anon-response-names.rds"))$names
  )

write_csv(
  temp_anon_resp,
  here("data", wave, paste0(wave, "_response-data-anon.csv"))
)


### 5 - Get opt outs ----

opt_outs <-
  here("data", wave, paste0(wave, "_opt-outs.xlsx")) %>%
  read_xlsx(sheet = 1) %>%
  select(email = `E-mail`) %>%
  add_cp_number(
    here("data", "registration-data",
         paste0(pre_wave, "_registration-data.rds")) %>%
      read_rds(),
    age_gender = TRUE,
    age_wave = wave,
    remove_email = TRUE
  )

# Save list of cp number, age and gender only
write_rds(
  opt_outs,
  here("data", wave, paste0(wave, "_opt-outs-anon.rds")),
  compress = "gz"
)


### END OF SCRIPT ###
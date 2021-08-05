#########################################################################
# Name of file - 01_clean-response-data.R
# Data release - Weekly CoMix Survey Analysis
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


### 1 - Save clean response data file ----

# Read in raw response data
resp <-
  here("data", "response-data",
       paste0(cur_wave, cur_panel, "_response-data-uncleaned.xlsx")) %>%
  read.xlsx(sheet = "Raw Data")


# Rename variables
names <- read_rds(here("lookups", "response-data-names.rds"))

names(resp) <- names$new_names


# Reformat variables

resp %<>% mutate_at(vars(matches("^new_hm[1-4]")), ~ as.character(.))


# Add CP Number

cp_number_lookup <-
  here("data", "registration-data",
       paste0(pre_wave, pre_panel, "_registration-data.rds")) %>%
  read_rds() %>%
  select(cp_number, email)

resp %<>%
  left_join(cp_number_lookup, by = "email") %>%
  select(cp_number, everything())


# Save clean response data as rds
write_rds(
  resp,
  here("data", "response-data",
       paste0(cur_wave, cur_panel, "_response-data.rds")),
  compress = "gz"
)


### 2 - Get household changes ----

hm_changes <-
  resp %>%
  filter(str_starts(hm_changes, "No")) %>%
  select(cp_number, hm_remove:new_hm4_student)

write_rds(
  hm_changes,
  here("data", "household-changes",
       paste0(cur_wave, cur_panel, "_hm-changes.rds")),
  compress = "gz"
)


### 3 - Anonymise response data ----

anon_resp <-
  resp %>%
  select(-email) %>%
  anon_response_data()

write_rds(
  anon_resp,
  here("data", "anon-data",
       paste0(cur_wave, cur_panel, "_response-data-anon.rds")),
  compress = "gz"
)


### 4 - Get opt outs ----

opt_outs <-
  here("data", "opt-outs", paste0(cur_wave, cur_panel, "_opt-outs.xlsx")) %>%
  read.xlsx(sheet = 1) %>%
  select(email = `E-mail`) %>%
  left_join(cp_number_lookup, by = "email") %>%
  select(-email)

# Save list of cp numbers only
write_rds(
  opt_outs,
  here("data", "opt-outs", paste0(cur_wave, cur_panel, "_opt-outs.rds")),
  compress = "gz"
)

# Delete opt out file with identifiable data
unlink(
  here("data", "opt-outs", paste0(cur_wave, cur_panel, "_opt-outs.xlsx"))
)


### END OF SCRIPT ###

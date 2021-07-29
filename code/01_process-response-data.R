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
# Description -
#########################################################################


### 0 - Setup ----

source(here::here("code", "00_setup.R"))


### 1 - Save clean response data file ----

# Read in raw response data
resp <-
  here("data", "response-data",
       paste0(wave, panel, "_response-data-uncleaned.xlsx")) %>%
  read.xlsx(sheet = "Raw Data")


# Rename variables
names <- read_rds(here("lookups", "response-data-names.rds"))

names(resp) <- names$new_names


# Add CP Number

cp_number_lookup <-
  here("data", "registration-data", "registration-data.rds") %>%
  read_rds() %>%
  select(cp_number, email)

resp %<>%
  left_join(cp_number_lookup, by = "email") %>%
  select(cp_number, everything())


# Save clean response data as rds
write_rds(
  resp,
  here("data", "response-data",
       paste0(wave, panel, "_response-data.rds")),
  compress = "gz"
)


### 2 - Get household changes ----

hh_changes <-
  resp %>%
  filter(str_starts(hm_changes, "No")) %>%
  select(cp_number, hm_remove:new_hm4_student)

write_rds(
  hh_changes,
  here("data", "household-changes", paste0(wave, panel, "_hh-changes.rds")),
  compress = "gz"
)


### 3 - Anonymise response data ----

anon_resp <-
  resp %>%
  mutate_at(
    vars(matches("new_hm[1-4]_name")),
    ~ if_else(!is.na(.x), "new-hm", NA_character_)) %>%
  mutate_at(
    vars(matches("c[1-30]")),
    ~ if_else(!is.na(.x), "contact", NA_character_)) %>%
  mutate_at(
    vars(matches("hm[1-11]_name")),
    ~ if_else(!is.na(.x), "hm", NA_character_))

write_rds(
  anon_resp,
  here("data", "anon-data", paste0(wave, panel, "_response-data-anon.rds")),
  compress = "gz"
)


### END OF SCRIPT ###

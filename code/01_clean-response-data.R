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
  select(email, hm_remove:new_hm4_student)

write_rds(
  hh_changes,
  here("data", "household-changes", paste0(wave, panel, "_hh-changes.rds")),
  compress = "gz"
)


### END OF SCRIPT ###

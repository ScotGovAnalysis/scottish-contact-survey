#########################################################################
# Name of file - 04_get-sample-data.R
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


### 1 - Get registration data ----

reg <-
  here("data", "registration-data",
       paste0(cur_wave, cur_panel, "_registration-data.rds")) %>%
  read_rds()


### 2 - Get data required for next wave QB invites ----

sample <-
  reg %>%
  filter(panel == cur_panel) %>%
  select(email, contains("_name"), employment_status, studying_location)


### 3 - Save data ---

write_csv(
  sample,
  here("data", "sample-data",
       paste0(cur_wave + 1, cur_panel, "_sample-data.csv"))
)


### END OF SCRIPT ###

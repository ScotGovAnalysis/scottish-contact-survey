#########################################################################
# Name of file - 02_update-registration-data.R
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


### 1 - Get data ----

reg <-
  here("data", "registraion-data", "registraion-data.rds") %>%
  read_rds()

hh_changes <-
  here("data", "household-changes", paste0(wave, panel, "_hh-changes.rds")) %>%
  read_rds()


### END OF SCRIPT ###

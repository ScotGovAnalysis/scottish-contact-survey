#########################################################################
# Name of file - 00_setup.R
# Data release - Weekly Scottish Contact Survey Analysis
# Original Authors - Alice Byers
# Original Date - July 2021
#
# Type - Reproducible Analytical Pipeline (RAP)
# Written/run on - RStudio Desktop
# Version of R - 3.6.3
#
# Description - Sets up environment required for running Scottish Contact
# Survey RAP. This is the only file which should require updating every
# time the RAP process is run.
#########################################################################

### 0 - Manual Variables - TO UPDATE ----

wave  <- 53

# If TRUE, opt-outs will be replaced in active panel by reserve list
add_reserves <- TRUE


### 1 - Load packages ----

library(dplyr)
library(readr)
library(readxl)
library(stringr)
library(magrittr)
library(scs)
library(purrr)
library(tidyr)
library(forcats)
library(lubridate)
library(here)
library(tibble)
library(usethis)
library(janitor)
library(rmarkdown)

options(readr.show_progress = FALSE)


### 2 - Derive previous/next wave and panel ----

pre_wave <- wave - 1

next_wave <- wave + 1


### 3 - Create data folders ----

c(wave, next_wave) %>%
  walk(~ use_directory(paste0("data/", .)))


### END OF SCRIPT ###
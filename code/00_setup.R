#########################################################################
# Name of file - 00_setup.R
# Data release - Weekly CoMix Survey Analysis
# Original Authors - Alice Byers
# Original Date - July 2021
#
# Type - Reproducible Analytical Pipeline (RAP)
# Written/run on - RStudio Desktop
# Version of R - 3.6.3
#
# Description - Sets up environment required for running CoMix RAP.
# This is the only file which should require updating every time the RAP
# process is run.
#########################################################################

### 0 - Manual Variables - TO UPDATE ----

cur_wave  <- 26
cur_panel <- "B"


### 1 - Load packages ----

library(dplyr)
library(readr)
library(openxlsx)
library(stringr)
library(magrittr)
library(comix)
library(purrr)
library(tidyr)
library(forcats)
library(lubridate)
library(here)


### 2 - Derive previous/next wave and panel ----

pre_wave <- case_when(
  cur_panel == "A" ~ cur_wave - 1,
  cur_panel == "B" ~ cur_wave
)

pre_panel <- setdiff(c("A", "B"), cur_panel)

next_wave <- case_when(
  cur_panel == "A" ~ cur_wave,
  cur_panel == "B" ~ cur_wave + 1
)

next_panel <- pre_panel


### END OF SCRIPT ###

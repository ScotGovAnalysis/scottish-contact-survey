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

### 0 - Manual Variable(s) - TO UPDATE ----

wave  <- 26
panel <- "A"


### 1 - Load packages ----

library(dplyr)
library(here)


### END OF SCRIPT ###

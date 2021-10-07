#########################################################################
# Name of file - 03_prep-next-wave.R
# Data release - Weekly CoMix Survey Analysis
# Original Authors - Alice Byers
# Original Date - August 2021
#
# Type - Reproducible Analytical Pipeline (RAP)
# Written/run on - RStudio Desktop
# Version of R - 3.6.3
#
# Description - Replace opt outs from reserve list and get information
# for Questback invites for next wave.
#########################################################################


### 0 - Setup ----

source(here::here("code", "00_setup.R"))


### 1 - Get registration data ----

reg <-
  here("data", "registration-data",
       paste0(cur_wave, cur_panel, "_registration-data.rds")) %>%
  read_rds()


### 2 - Replace opt outs ----

# # Get data for people on reserve list
# reserve_data <-
#   reg %>%
#   mutate(age_group = age_group(age(date_of_birth, cur_wave, cur_panel))) %>%
#   filter(status == "reserve") %>%
#   select(email, age_group, gender)
#
# # Get data for opt outs
# opt_out_data <-
#   here("data", "opt-outs", paste0(cur_wave, cur_panel, "_opt-outs.rds")) %>%
#   read_rds() %>%
#   count(age_group, gender) %>%
#   rename(n_opt_outs = n)
#
# # Get maximum CP number for current panel
# max_cp <-
#   reg %>%
#   filter(str_starts(cp_number, paste0("CP", cur_panel))) %>%
#   mutate(cp = str_remove(cp_number, paste0("CP", cur_panel))) %$%
#   as.numeric(max(cp))
#
# # Get list of emails and new cp numbers for replacement
# replace <-
#   tibble(
#     email = replace_opt_outs(reserve_data, opt_out_data)
#   ) %>%
#   mutate(new_cp = paste0(
#     "CP", cur_panel, seq(from = max_cp + 1, by = 1, length.out = nrow(.))
#   ))
#
# # Update registration data to add replacements to current panel
# reg %<>%
#   left_join(replace, by = "email") %>%
#   mutate(
#     panel = if_else(!is.na(new_cp), cur_panel, panel),
#     status = if_else(!is.na(new_cp), "active", status),
#     cp_number = if_else(!is.na(new_cp), new_cp, cp_number)
#   ) %>%
#   select(-new_cp)


### 3 - Save updated registration data ----

write_rds(
  reg,
  here("data", "registration-data",
       paste0(next_wave, next_panel, "_registration-data.rds")),
  compress = "gz"
)


### 4 - Get data required for next wave QB invites ----

invites <-
  reg %>%
  filter(status == "active" & panel == cur_panel) %>%
  select(email, contains("_name"), employment_status, studying_location)


### 5 - Save data for upload to Questback ---

# Temp - reformat as required for controller script
# Future work will incorporate controllor script into comix package
# This section can be dropped once this is done.

temp_invites <-
  invites %>%
  add_column(x1 = NA, x2 = NA, .after = "hm10_name") %>%
  add_column(x3 = NA, x4 = NA, .after = "employment_status") %>%
  inset(sprintf("x%d", 18:69), value = NA) %>%
  set_colnames(read_rds(here("lookups", "questback-invite-names.rds"))$names)

write_csv(
  temp_invites,
  here("data", "questback-invites",
       paste0(cur_wave + 1, cur_panel, "_qb-invites.csv")),
  na = ""
)


### END OF SCRIPT ###
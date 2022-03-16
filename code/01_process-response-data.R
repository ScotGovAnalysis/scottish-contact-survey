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
  here("data", cur_survey,
       paste0(cur_survey, "_response-data.csv")) %>%
  read_csv(col_types = paste(resp_names$type, collapse = "")) %>%

  # Clean names
  set_names(resp_names$new_names) %>%

  # Format date
  mutate(date_completed = dmy_hms(date_completed)) %>%

  # Remove responses where consent not given or not in scotland
  filter(!is.na(consent) & in_scotland == "Yes")


# Add CP Number

cp_number_lookup <-
  here("data", "registration-data",
       paste0(cur_survey, "_registration-data.rds")) %>%
  read_rds() %>%
  select(cp_number, email, date_of_birth, gender)

resp %<>%
  left_join(cp_number_lookup %>% select(cp_number, email), by = "email") %>%
  select(cp_number, everything())


### 2 - Run prize draw ----

winner <-
  resp %>%
  select(cp_number, email) %>%
  sample_n(1)

write_csv(
  winner,
  here("data", cur_survey, paste0(cur_survey, "_prize-draw.csv"))
)


### 3 - Get household changes ----

hm_removed <-
  household_changes(resp, "remove") %T>%
  write_rds(
    here("data", cur_survey, paste0(cur_survey, "_hm-removed.rds")),
    compress = "gz"
  )

hm_added <-
  household_changes(resp, "add") %T>%
  write_rds(
    here("data", cur_survey, paste0(cur_survey, "_hm-added.rds")),
    compress = "gz"
  )


### 4 - Anonymise response data ----

anon_resp <-
  resp %>%
  select(-email) %>%
  anon_response_data()

write_rds(
  anon_resp,
  here("data", cur_survey, paste0(cur_survey, "_response-data-anon.rds")),
  compress = "gz"
)

# Save backup
write_rds(
  anon_resp,
  paste0("//s0177a/datashare/CoMix/Private/CoMix Model/Backup Data/",
         cur_survey, "_response-data-anon.rds"),
  compress = "gz"
)

# Temp - reformat as required for controller script
# Future work will incorporate controllor script into comix package
# This section can be dropped once this is done.

temp_anon_resp <- anon_resp %>%

  # Temp - remove some columns
  select(-in_scotland, -matches("^updated_"), -c(vacc_1:hm14_test_positive),
         -lateral_flow_stock, -matches("^visit_healthcare_"),
         -vaccine, -to_update, -household_members,
         -matches("^covid_(un)?confirmed"), -time_since_covid_unconfirmed) %>%

  # Temp - add empty columns and rearrange
  add_column(hm11_change = NA, .after = 16) %>%
  magrittr::inset(sprintf("temp_%d", 1:245), value = NA) %>%
  magrittr::extract(, c(1:39, 1430:1674, 40:1429)) %>%
  add_column(hm15_contact = NA, .after = 332) %>%
  magrittr::inset(sprintf("temp_%d", 246:(246+26)), value = NA) %>%
  magrittr::extract(, c(1:741, 1676:1702, 742:1675)) %>%
  magrittr::extract(, c(1:1690, 1693:1702, 1691:1692)) %>%
  add_column(hm11_name = NA, employment_1_0 = NA, .after = 1700) %>%
  add_column(children_1 = NA, children_2 = NA, .after = 1703) %>%
  magrittr::inset(sprintf("end_%d", 1:56), value = NA) %>%

  # Temp - rename variables for controller script
  set_names(read_rds(here("lookups", "anon-response-names.rds"))$names)

write_csv(
  temp_anon_resp,
  here("data", cur_survey, paste0(cur_survey, "_response-data-anon.csv"))
)


### 5 - Get opt outs ----

opt_outs <-
  here("data", cur_survey, paste0(cur_survey, "_opt-outs.xlsx")) %>%
  read.xlsx(sheet = 1) %>%
  select(email = `E-mail`) %>%
  left_join(cp_number_lookup, by = "email") %>%
  mutate(age_group = age_group(age(date_of_birth, cur_wave, cur_panel))) %>%
  select(-email, -date_of_birth)

# Save list of cp number, age and gender only
write_rds(
  opt_outs,
  here("data", cur_survey, paste0(cur_survey, "_opt-outs-anon.rds")),
  compress = "gz"
)


### END OF SCRIPT ###
#########################################################################
# Name of file - 02_update-registration-data.R
# Data release - Weekly Scottish Contact Survey Analysis
# Original Authors - Alice Byers
# Original Date - July 2021
#
# Type - Reproducible Analytical Pipeline (RAP)
# Written/run on - RStudio Desktop
# Version of R - 3.6.3
#
# Description - Remove personal data for opt outs and update household
# information in registration data.
#########################################################################


### 0 - Setup ----

source(here::here("code", "00_setup.R"))


### 1 - Get data ----

reg <-
  here("data", "registration-data",
       paste0(cur_survey, "_registration-data.rds")) %>%
  read_rds()


### 2 - Recode opt outs ----

# For opt outs, keep CP number and remove all other registration data

reg %<>%
  recode_opt_outs(
    here("data", cur_survey, paste0(cur_survey, "_opt-outs-anon.rds")) %>%
      read_rds() %>%
      pull(cp_number)
  )


### 3 - Get data required for household changes ----

# Keep only active participants in panel
reg_active <- reg %>%
  filter(status == "active" & panel == cur_panel)


hm_changes <-

  # Get household changes from latest survey
  here("data", cur_survey, paste0(cur_survey, "_hm-changes.rds")) %>%
  read_rds() %>%

  # Get total number of household members with removals and
  # total number of new household members
  mutate(
    n_old_hm = reduce(select(., matches("^hm([1-9]|10)_change$")), `+`) + 1,
    n_new_hm = reduce(select(., matches("^new_hm[1-4]_name")) %>%
                            mutate_all(~!is.na(.)), `+`)
  ) %>%

  # Get number of household members removed compared to reg data
  left_join(
    reg_active %>% select(cp_number, n_household), by = "cp_number"
  ) %>%
  mutate(n_removed = n_household - n_old_hm)


### 4 - Remove household members ----

remove <-

  # Get changes for households with members removed
  hm_changes %>%
  filter(hm_remove == 1 & n_removed > 0) %>%
  select(cp_number, hm1_change:hm11_change) %>%

  # Restrucutre to long format
  pivot_longer(cols = hm1_change:hm11_change,
               names_to = "hm",
               values_to = "change") %>%
  mutate(hm = str_remove(hm, "_change"))


hm_removed <-

  # Get households from registration data
  reg_active %>%
  select(cp_number, matches("^hm([1-9]|10)")) %>%

  # Restructure to long format
  pivot_longer(cols = hm1_name:hm10_student,
               names_to = c("hm", "dem"),
               names_sep = "_",
               values_to = "dat") %>%

  # Match on hm changes and remove household members
  left_join(remove, by = c("cp_number", "hm")) %>%
  group_by(cp_number, hm) %>%
  mutate(dat = case_when(
    any(change == 0) ~ NA_character_,
    TRUE ~ dat
  )) %>%
  ungroup() %>%
  select(-change) %>%

  # Update household positions;
  # e.g. If HM #2 removed, move #3 to position #2
  group_by(cp_number, hm) %>%
  mutate(sort = max(dem == "name" & is.na(dat))) %>%
  arrange(sort) %>%
  group_by(cp_number) %>%
  mutate(hm = rep(paste0("hm", 1:10), each = 5)) %>%
  select(-sort) %>%

  # Get updated number of household members
  group_by(cp_number) %>%
  mutate(n_hm = max(case_when(
    dem == "name" & !is.na(dat) ~ parse_number(hm),
    TRUE ~ 0
  )))


n_hm <-
  hm_removed %>%
  group_by(cp_number) %>%
  summarise(n_hm = max(n_hm)) %>%
  ungroup()



### 5 - Add new household members ---

add <-

  # Get changes for households with members added
  hm_changes %>%
  filter(hm_add == 1 & n_new_hm > 0) %>%
  select(cp_number, new_hm1_name:new_hm4_student) %>%

  # Restrucutre to long format
  rename_at(vars(new_hm1_name:new_hm4_student), ~ str_remove(., "new_")) %>%
  pivot_longer(cols = hm1_name:hm4_student,
               names_to = c("hm", "dem"),
               names_sep = "_",
               values_to = "dat") %>%

  # Remove empty HM positions
  group_by(cp_number, hm) %>%
  filter(
    max(case_when(
      dem == "name" & is.na(dat) ~ 1,
      TRUE ~ 0
    )) == 0) %>%
  ungroup() %>%

  # Update household member number
  left_join(n_hm, by = "cp_number") %>%
  mutate(hm = paste0("hm", parse_number(hm) + n_hm)) %>%
  select(-n_hm)


hm_added <-

  # Add new household members
  hm_removed %>%
  select(-n_hm) %>%
  bind_rows(add %>% mutate(flag = "new_hm")) %>%

  # Remove empty rows for HM positions now filled
  group_by(cp_number, hm, dem) %>%
  filter(n() == 1 |
           n() > 1 & flag == "new_hm") %>%
  ungroup() %>%
  select(-flag) %>%

  # Sort
  mutate(hm = fct_relevel(as_factor(hm), paste0("hm", 1:10))) %>%
  arrange(cp_number, hm) %>%

  # Restructure to wide format
  pivot_wider(names_from = c(hm, dem),
              names_sep = "_",
              values_from = dat)


### 6 - Add updated household data back into registration data

new_reg <-
  reg_active %>%
  select(-names(hm_added)[-1]) %>%
  left_join(hm_added, by = "cp_number") %>%
  select(names(reg)) %>%
  bind_rows(reg %>% filter(!(status == "active" & panel == cur_panel))) %>%

  # Update household count
  mutate(n_household = reduce(select(., matches("^hm([1-9]|10)_name$")) %>%
                                mutate_all(~ !is.na(.)), `+`) + 1)


### 7 - Save updated registration data ----

write_rds(
  new_reg,
  here("data", "registration-data",
       paste0(cur_survey, "_registration-data.rds")),
  compress = "gz"
)


### 8 - Save anonymised registration data for current wave ----

anon_reg <-
  new_reg %>%
  filter(status == "active" & panel == cur_panel) %>%
  select(-email) %>%
  anon_reg_data()

write_rds(
  anon_reg,
  here("data", cur_survey, paste0(cur_survey, "_registration-data-anon.rds")),
  compress = "gz"
)

# Save backup
write_rds(
  anon_reg,
  paste0("//s0177a/datashare/CoMix/Private/CoMix Model/Backup Data/",
         cur_survey, "_registration-data-anon.rds"),
  compress = "gz"
)

# Temp - reformat as required for controller script
# Future work will incorporate controllor script into comix package
# This section can be dropped once this is done.

temp_anon_reg <- anon_reg %>%

  # Temp - add missing columns and reorder
  select(-status, -panel, -local_authority_code) %>%
  add_column(hh_changes = NA, .after = "cp_number") %>%
  add_column(ethnicity2 = NA, .after = "ethnicity") %>%
  select(cp_number:n_household,
         matches("hm\\d{1,2}_name"), everything()) %>%
  add_column(
    `12.1` = NA, `12.2` = NA, `12.3` = NA,
    `12.4` = NA, `12.5` = NA, `12.6` = NA,
    `12.7` = NA, `12.8` = NA, `12.9` = NA,
    `12.10` = NA, `12.11` = NA, `12.12` = NA,
    .after = "hm10_name"
  ) %>%
  select(cp_number:`12.12`,
         employment_status:total_household_income,
         everything()) %>%
  mutate(date_of_birth = age(date_of_birth, cur_wave, cur_panel)) %>%

  # Temp - rename variables for controller script
  set_names(read_rds(here("lookups", "anon-sample-names.rds"))$names)

write_csv(
  temp_anon_reg,
  here("data", cur_survey, paste0(cur_survey, "_registration-data-anon.csv"))
)


### END OF SCRIPT ###
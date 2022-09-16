#########################################################################
# Name of file - resp-names.R
# Original Authors - Alice Byers
# Original Date - September 2022
#
# Description - Creates dataset containing all variable names and types
# for survey response data.
#########################################################################


# Create tibble with column of response data variable names

resp_names <- tibble::tibble(

  names = c(

    "email", "date_completed", "consent", "in_scotland",
    "hm_changes", "hm_remove", "hm_add",

    paste0("hm", 1:10, "_change"),

    "no_hm", "new_hm_count",

    purrr::pmap_chr(
      expand.grid(c("name", "age", "gender", "occupation", "student"), 1:4),
      ~ paste0("new_hm", .y, "_", .x)
    ),

    "updated_postcode", "updated_local_authority", "updated_employment_status",
    "updated_studying", "updated_studying_location", "updated_employed",
    "updated_highest_earner_flag", "updated_occupation_1",
    "updated_occupation_2", "updated_highest_earner_occupation_1",
    "updated_highest_earner_occupation_2", "updated_total_household_income",
    "vacc_1", "vacc_2",
    "lateral_flow", "reported_lateral_flow", "test_positive",

    paste0("hm", 1:14, "_test_positive"),

    "covid_unconfirmed", "time_since_covid_unconfirmed", "covid_confirmed",

    "workplace", "workplace_7days", "workplace_yesterday",
    "education", "education_7days", "education_yesterday",

    "visit_other_house", "visit_worship", "visit_public_transport",
    "visit_supermarket", "visit_non_essential_shop", "visit_entertainment",
    "visit_sports", "visit_outside", "visit_healthcare", "visit_hairdresser",

    "visit_healthcare_gp", "visit_healthcare_dentist",
    "visit_healthcare_optometrist", "visit_healthcare_ae",
    "visit_healthcare_outpatient", "visit_healthcare_inpatient",
    "visit_healthcare_vaccine", "visit_healthcare_physio",
    "visit_healthcare_pharmacy", "visit_healthcare_work",
    "visit_healthcare_other",

    "face_mask", "fm_outside", "fm_street", "fm_cycling", "fm_public_transport",
    "fm_supermarket", "fm_entertainment", "fm_home", "fm_other_house",
    "fm_education", "fm_park", "fm_other",

    "public_transport", "train", "bus", "taxi", "aeroplane", "ferry",

    paste0("hm", 1:14, "_contact"),

    paste0("c", 1:30),

    purrr::pmap_chr(
      expand.grid(
        c("home", "other_house", "work", "worship", "public_transport",
          "education", "supermarket", "non-essential", "entertainment",
          "sports", "park", "healthcare", "hairdresser",
          "other", "inside", "outside"),
        1:14),
      ~ paste0("hm", .y, "_", .x)
    ),

    purrr::pmap_chr(
      expand.grid(
        c("age", "gender", "home", "other_house", "work", "worship",
          "public_transport", "education", "supermarket", "non-essential",
          "entertainment", "sports", "park", "healthcare", "hairdresser",
          "other", "inside", "outside"),
        1:30
      ),
      ~ paste0("c", .y, "_", .x)
    ),

    "additional_contacts",

    purrr::pmap_chr(
      expand.grid(
        age = c("under_18", "18_64", "65_over"),
        contact = c("direct", "physical"),
        setting = c("work", "education", "other")
      ),
      ~ paste(..3, ..2, ..1, sep = "_")
    ),

    "employment", "studying", "vaccine", "to_update",

    paste0("hm", 1:10, "_name"),

    "household_members"

  )

)


# Define vector of character variables

char_types <- c(

  "email", "date_completed", "consent",
  "in_scotland", "hm_changes", "new_hm_count",

  purrr::pmap_chr(
    expand.grid(c("name", "age", "gender", "occupation", "student"), 1:4),
    ~ paste0("new_hm", .y, "_", .x)
  ),

  "updated_postcode", "updated_local_authority", "updated_employment_status",
  "updated_studying", "updated_studying_location", "updated_employed",
  "updated_highest_earner_flag", "updated_occupation_1", "updated_occupation_2",
  "updated_highest_earner_occupation_1", "updated_highest_earner_occupation_2",
  "updated_total_household_income", "vacc_1", "vacc_2",
  "lateral_flow", "reported_lateral_flow", "test_positive",

  paste0("hm", 1:14, "_test_positive"),

  "covid_unconfirmed", "time_since_covid_unconfirmed", "covid_confirmed",
  "workplace", "workplace_7days", "workplace_yesterday",
  "education", "education_7days", "education_yesterday",
  "visit_healthcare_other",
  "face_mask", "fm_other",
  "public_transport",

  paste0("c", 1:30),

  paste0("hm", 1:14, "_other"),

  purrr::pmap_chr(
    expand.grid(
      c("age", "gender", "other"),
      1:30
    ),
    ~ paste0("c", .y, "_", .x)
  ),

  "additional_contacts",
  "employment", "studying", "vaccine",

  paste0("hm", 1:10, "_name")

)


# Add column with variable type

resp_names <-
  resp_names %>%
  mutate(type = if_else(names %in% char_types, "c", "n"))


# Save to data/
usethis::use_data(resp_names, overwrite = TRUE)


### END OF SCRIPT ###
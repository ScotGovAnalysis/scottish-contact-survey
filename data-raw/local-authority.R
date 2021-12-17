#########################################################################
# Name of file - local-authority.R
# Original Authors - Alice Byers
# Original Date - December 2021
#
# Description - Creates internal local authority code to name lookup.
#########################################################################


la <-
  paste0("https://www.opendata.nhs.scot/dataset/9f942fdb-e59e-44f5-b534-d6e",
         "17229cc7b/resource/967937c4-8d67-4f39-974f-fd58c4acfda5/download/",
         "ca11_ca19.csv") %>%
  readr::read_csv() %>%
  janitor::clean_names() %>%
  dplyr::filter(is.na(ca_date_archived)) %>%
  dplyr::count(ca, ca_name) %>%
  dplyr::select(local_authority_code = ca, local_authority = ca_name)

usethis::use_data(la, internal = TRUE, overwrite = TRUE)


### END OF SCRIPT ###
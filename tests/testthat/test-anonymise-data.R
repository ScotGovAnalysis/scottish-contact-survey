
# Create test data

`%>%` <- magrittr::`%>%`

reg_names <- paste0("hm", 1:10, "_name")
resp_names <- c(reg_names, paste0("new_hm", 1:4, "_name"), paste0("c", 1:30))

reg <-
  matrix(nrow = 100, ncol = length(reg_names)) %>%
  tibble::as_tibble(.name_repair = ~ reg_names) %>%
  dplyr::mutate(dplyr::across(
    tidyselect::everything(),
    ~ sample(c(paste("non-anon", dplyr::cur_column()), NA), 100, replace = TRUE)
  )) %>%
  dplyr::mutate(id = dplyr::row_number(), .before = tidyselect::everything())

resp <-
  matrix(nrow = 100, ncol = length(resp_names)) %>%
  tibble::as_tibble(.name_repair = ~ resp_names) %>%
  dplyr::mutate(dplyr::across(
    tidyselect::everything(),
    ~ sample(c(paste("non-anon", dplyr::cur_column()), NA), 100, replace = TRUE)
  )) %>%
  dplyr::mutate(id = dplyr::row_number(), .before = tidyselect::everything())

reg_anon <- anonymise_data(reg, "reg")
resp_anon <- anonymise_data(resp, "resp")


# Run tests

test_that("Error if expected variables don't exist in data", {
  expect_error(anonymise_data(reg, "resp"))
})

test_that("Error if invalid dataset_to_anon value provided", {
  expect_error(anonymise_data(reg, "wrong dataset"))
})

test_that("Dimensions of returned tibble are the same as input tibble", {
  expect_equal(dim(reg), dim(reg_anon))
  expect_equal(dim(resp), dim(resp_anon))
})

test_that("Names of returned tibble are the same as input tibble", {
  expect_equal(names(reg), names(reg_anon))
  expect_equal(names(resp), names(resp_anon))
})

test_that("Variables not to be anonymised remain unchanged", {
  expect_equal(reg$id, reg_anon$id)
  expect_equal(resp$id, resp_anon$id)
})

test_that("Name/nickname variables are anonymised correctly", {
  expect_equal(sort(unique(reg_anon$hm1_name), na.last = TRUE),
               c("HM1", NA_character_))
  expect_equal(sort(unique(resp_anon$new_hm1_name), na.last = TRUE),
               c("New HM1", NA_character_))
  expect_equal(sort(unique(resp_anon$c10), na.last = TRUE),
               c("C10", NA_character_))
})

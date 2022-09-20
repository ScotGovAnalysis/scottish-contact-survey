

reg_anon <- anonymise_data(dummy_reg, "reg")
resp_anon <- anonymise_data(dummy_resp, "resp")


test_that("Error if expected variables don't exist in data", {
  expect_error(anonymise_data(dummy_reg, "resp"))
})

test_that("Error if invalid dataset_to_anon value provided", {
  expect_error(anonymise_data(dummy_reg, "wrong dataset"))
})

test_that("Dimensions of returned tibble are the same as input tibble", {
  expect_equal(dim(dummy_reg), dim(reg_anon))
  expect_equal(dim(dummy_resp), dim(resp_anon))
})

test_that("Names of returned tibble are the same as input tibble", {
  expect_equal(names(dummy_reg), names(reg_anon))
  expect_equal(names(dummy_resp), names(resp_anon))
})

test_that("Variables not to be anonymised remain unchanged", {
  expect_equal(dummy_reg$cp_number, reg_anon$cp_number)
  expect_equal(dummy_resp$email, resp_anon$email)
})

test_that("Name/nickname variables are anonymised correctly", {
  expect_equal(sort(unique(reg_anon$hm1_name), na.last = TRUE),
               c("HM1", NA_character_))
  expect_equal(sort(unique(resp_anon$new_hm1_name), na.last = TRUE),
               c("New HM1", NA_character_))
  expect_equal(sort(unique(resp_anon$c10), na.last = TRUE),
               c("C10", NA_character_))
})


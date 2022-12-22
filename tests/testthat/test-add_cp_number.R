
dummy_resp_no_cp <- dummy_resp %>% dplyr::select(-cp_number)

cp <- add_cp_number(dummy_resp_no_cp, dummy_reg)
cp_age <- add_cp_number(dummy_resp_no_cp, dummy_reg,
                        age_gender = TRUE, age_wave = 55)
no_email <- add_cp_number(dummy_resp_no_cp, dummy_reg, remove_email = TRUE)


test_that("Error if required variables don't exist in either dataset", {
  expect_error(add_cp_number(dummy_resp_no_cp %>% dplyr::select(-email),
                             dummy_reg))
  expect_error(add_cp_number(dummy_resp_no_cp,
                             dummy_reg %>% dplyr::select(-email)))
  expect_error(add_cp_number(dummy_resp_no_cp,
                             dummy_reg %>%
                               dplyr::select(-date_of_birth, -gender),
                             age_gender = TRUE,
                             age_wave = 55))
})

test_that("Error if variables already exist in response data", {
  expect_error(add_cp_number(dummy_resp, dummy_reg))
  expect_error(add_cp_number(dummy_resp_no_cp %>%
                               tibble::add_column(age_group = "",
                                                  gender = ""),
                             dummy_reg,
                             age_gender = TRUE,
                             age_wave = 55))
})

test_that("Error if email doesn't exist in registration data", {
  test_email <- dummy_resp$email[1]
  expect_error(add_cp_number(dummy_resp_no_cp,
                             dummy_reg %>% dplyr::filter(email != test_email)))
})

test_that("Error if wave/panel not supplied", {
  expect_error(add_cp_number(dummy_resp_no_cp,
                             dummy_reg,
                             age_gender = TRUE))
  expect_error(add_cp_number(dummy_resp_no_cp,
                             dummy_reg,
                             age_gender = TRUE,
                             age_wave = 40))
})

test_that("Warning if panel supplied when not needed", {
  expect_warning(add_cp_number(dummy_resp_no_cp,
                               dummy_reg,
                               age_gender = TRUE,
                               age_wave = 55,
                               age_panel = "A"))
})

test_that("Tibble returned", {
  expect_true(tibble::is_tibble(cp))
})

test_that("Correct variables returned", {
  expect_true("cp_number" %in% names(cp))
  expect_true(all(!c("age_group", "gender") %in% names(cp)))
  expect_true(all(c("cp_number", "age_group", "gender") %in% names(cp_age)))
  expect_true(!"email" %in% names(no_email))
})

test_that("CP Number matched correctly", {
  expect_equal(stringr::str_remove(cp$email, "@email.com"), cp$cp_number)
})


add <- household_changes(dummy_resp, "add")
remove <- household_changes(dummy_resp, "remove")
reg_updated <- suppressWarnings(
  update_household_members(dummy_reg, to_remove = remove, to_add = add)
)


test_that("Error if no household change data supplied", {
  expect_error(update_household_members(dummy_reg))
})

test_that("Error if expected variables not in `to_add` or `to_remove`", {

  expect_error(update_household_members(dummy_reg,
                                        to_add = subset(add, select = -hm_add)))
  expect_error(
    update_household_members(dummy_reg,
                             to_remove = subset(remove, select = -cp_number))
  )

})

test_that("Returned value is a tibble with expected column names", {
  expect_true(tibble::is_tibble(reg_updated))
  expect_equal(dim(dummy_reg), dim(reg_updated))
  expect_equal(names(dummy_reg), names(reg_updated))
})

test_that("Warning if more than 10 household members", {

  add_10plus <- dummy_reg %>%
    dplyr::filter(cp_number %in% add$cp_number & n_household == 11) %>%
    magrittr::extract2(1, 1)

  expect_warning(
    update_household_members(
      dummy_reg, to_add = dplyr::filter(add, cp_number == add_10plus)
    )
  )

})


test_that("Correct value returned", {

  add_summary <-
    add %>%
    dplyr::group_by(cp_number) %>%
    dplyr::summarise(n_add = dplyr::n(), .groups = "drop")

  remove_summary <-
    remove %>%
    dplyr::group_by(cp_number) %>%
    dplyr::summarise(n_remove = dplyr::n(), .groups = "drop")

  existing_summary <-
    dummy_reg %>%
    dplyr::filter(cp_number %in% c(add$cp_number, remove$cp_number)) %>%
    dplyr::select(cp_number, n_hm = n_household)

  updated_summary <-
    reg_updated %>%
    dplyr::filter(cp_number %in% c(add$cp_number, remove$cp_number)) %>%
    dplyr::select(cp_number, n_hm_updated = n_household)

  check <-
    existing_summary %>%
    dplyr::left_join(add_summary, by = "cp_number") %>%
    dplyr::left_join(remove_summary, by = "cp_number") %>%
    dplyr::left_join(updated_summary, by = "cp_number") %>%
    dplyr::mutate(dplyr::across(!cp_number, ~ tidyr::replace_na(., 0))) %>%
    dplyr::mutate(n_hm_expected = n_hm + n_add - n_remove,
                  n_hm_expected = ifelse(n_hm_expected > 11, 11, n_hm_expected))

  expect_identical(check$n_hm_updated, check$n_hm_expected)

})

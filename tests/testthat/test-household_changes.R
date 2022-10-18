
test_that("Error if invalid change_type supplied", {
  expect_error(household_changes(dummy_resp, "invalid_change"))
})

test_that("Error if expected variables not in resp_data", {
  expect_error(household_changes(subset(dummy_resp, select = -cp_number),
                                 "add"))
})

test_that("Correct information returned for household members to be added", {

  add <- household_changes(dummy_resp, "add")

  x <-
    dummy_resp %>%
    dplyr::filter(hm_add == 1) %>%
    dplyr::mutate(
      dplyr::across(tidyselect::matches("^new_hm[1-4]_name$"),
                    ~ as.numeric(!is.na(.))),
      n_add = new_hm1_name + new_hm2_name + new_hm3_name + new_hm4_name
    ) %>%
    dplyr::filter(n_add >= 1) %>%
    dplyr::select(cp_number, tidyselect::matches("^new_hm[1-4]_name$"), n_add)

  expect_named(add,
               c("cp_number", "hm_add", "name", "age", "gender",
                 "occupation", "student"))

  expect_match(add$hm_add, "^hm[1-4]$")

  expect_equal(dplyr::n_distinct(add$cp_number),
               dplyr::n_distinct(x$cp_number))

  expect_equal(nrow(add), sum(x$n_add))

})

test_that("Correct information returned for household members to be removed", {

  remove <- household_changes(dummy_resp, "remove")

  y <-
    dummy_resp %>%
    dplyr::filter(hm_remove == 1) %>%
    dplyr::select(cp_number,
                  tidyselect::matches("^hm\\d{1,2}_change$"),
                  tidyselect::matches("^hm\\d{1,2}_name$")) %>%
    tidyr::pivot_longer(cols = !cp_number,
                        names_to = c("hm", ".value"),
                        names_sep = "_") %>%
    dplyr::group_by(cp_number) %>%
    dplyr::summarise(change = sum(change, na.rm = TRUE),
                     name = sum(!is.na(name)),
                     .groups = "drop") %>%
    dplyr::filter(change < name) %>%
    dplyr::mutate(n_remove = name - change)

  expect_named(remove, c("cp_number", "hm_remove"))

  expect_match(remove$hm_remove, "^hm([1-9]|10)$")

  expect_equal(dplyr::n_distinct(remove$cp_number),
               dplyr::n_distinct(y$cp_number))

  expect_equal(nrow(remove), sum(y$n_remove))

})

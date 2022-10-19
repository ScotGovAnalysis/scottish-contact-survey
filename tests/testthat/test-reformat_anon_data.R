
test_that("`add_cols` returns correct value", {

  expect_named(add_cols(mtcars, 2, "carb"),
               c(names(mtcars), "temp_1", "temp_2"))

  expect_true(all(is.na(add_cols(mtcars, 1, "mpg")$temp_1)))

  x <- mtcars
  x$temp_1 <- NA

  expect_named(add_cols(x, 1, "carb"),
               c(names(mtcars), "temp_2", "temp_1"))

})

test_that("Error if invalid argument format", {
  expect_error(assign_names(c("A", "B"), c("A", "B")))
  expect_error(assign_names(data.frame(x = 1:5, y = 1:5), 1:5))
  expect_error(assign_names(data.frame(x = 1:5, y = 1:5), "A"))
})

test_that("Error if number of columns in dataset and names don't match", {
  expect_error(data.frame(x = 1:5, y = 1:5) %>% assign_names(c("A")))
})

test_that("`assign_names` returns correct value", {

  orig <- data.frame(x = 1:5, y = 6:10)
  renamed <- assign_names(orig, c("a", "b"))

  expect_true(is.data.frame(renamed))
  expect_named(renamed, c("a", "b"))
  expect_equal(unname(orig), unname(renamed))

})

test_that("Error if names not in character format", {

  expect_error(reformat_anon_resp(dummy_resp, names = 1:5))
  expect_error(reformat_anon_reg(dummy_reg, names = 1:5,
                                 wave = 30, panel = "A"))

})

test_resp <- subset(dummy_resp, select = -email)
test_reg <- subset(dummy_reg, select = -email)

test_that("Error/warning if wave and panel invalid", {

  expect_error(reformat_anon_reg(test_reg, names = "names",
                                 wave = "1A", panel = "A"))

  expect_error(reformat_anon_reg(test_reg, names = "names",
                                 wave = 30, panel = NULL))

  expect_error(reformat_anon_reg(test_reg, names = "names",
                                 wave = 30, panel = "invalid panel"))

  expect_warning(reformat_anon_reg(test_reg, names = paste0("name", 1:87),
                                   wave = 50, panel = "A"))

})

test_that("Return value in correct format", {

  reg <- reformat_anon_reg(test_reg, names = paste0("name", 1:87), wave = 50)
  resp <- reformat_anon_resp(test_resp, names = paste0("name", 1:1762))

  expect_true(is.data.frame(reg))
  expect_true(is.data.frame(resp))
  expect_equal(dim(reg), c(nrow(test_reg), 87))
  expect_equal(dim(resp), c(nrow(test_resp), 1762))

})

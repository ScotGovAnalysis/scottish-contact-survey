
test_that("Error if survey in wrong format", {
  expect_error(build_filepath(filename = "file1",
                              survey = "invalid",
                              create_folder = FALSE))
  expect_error(build_filepath(filename = "file1",
                              survey = "500",
                              create_folder = FALSE))
  expect_error(build_filepath(filename = "file1",
                              survey = "5C",
                              create_folder = FALSE))
})

test_that("Error if data_folder doesn't exist", {
  expect_error(build_filepath(filename = "file1",
                              survey = "50",
                              data_folder = "/fake/folder/",
                              create_folder = FALSE))
})

test_that("Error if create_folder isn't logical", {
  expect_error(build_filepath(filename = "file1",
                              survey = "50",
                              create_folder = "TRUE"))
})

# Create temp directory to act as data folder
dir <- tempdir(check = TRUE)

test_that("Warning if create_folder = FALSE and folder doesn't exist", {
  expect_warning(build_filepath(filename = "file1",
                                survey = "50",
                                data_folder = dir,
                                create_folder = FALSE))
})

test_that("Function returns correct value", {

  x <- suppressWarnings(build_filepath(filename = "file1",
                                       survey = "50",
                                       data_folder = dir,
                                       create_folder = FALSE))

  expect_equal(x, paste0(dir, "/50/50_file1.rds"))
  expect_false(dir.exists(paste0(dir, "/50/")))

  y <- build_filepath(filename = "file2",
                      survey = "55",
                      fileext = "csv",
                      data_folder = dir,
                      create_folder = TRUE)

  expect_equal(y, paste0(dir, "/55/55_file2.csv"))
  expect_true(dir.exists(paste0(dir, "/55/")))

  z <- build_filepath(filename = paste0("file", 1:5),
                      survey = "6A",
                      data_folder = dir)

  expect_equal(z, paste0(dir, "/6A/6A_file", 1:5, ".rds"))

})


dir2 <- tempdir(check = TRUE)

test_that("Only first value used for certain arguments", {

  w <- suppressWarnings(build_filepath(filename = c("test1", "test2"),
                                       survey = c("20A", "20B"),
                                       fileext = c(".csv", ".xlsx"),
                                       data_folder = c(dir, dir2),
                                       create_folder = c(FALSE, TRUE)))

  expect_equal(w, paste0(dir, "/20A/20A_", c("test1", "test2"), ".csv"))
  expect_false(dir.exists(paste0(dir, "/20A/")))

})

# Delete temp directory
unlink(c(dir, dir2), recursive = TRUE)


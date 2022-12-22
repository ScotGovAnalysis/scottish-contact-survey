
test_that("Error if data_folder doesn't exist", {
  expect_error(
    delete_files(".txt", data_folder = tempfile(), user_confirm = FALSE)
  )
})

folder <- tempdir(check = TRUE)
files  <- paste0("temp", 1:5, ".txt")
filepaths <- paste0(folder, "\\", files)
file.create(filepaths)

test_that("Error if no files exist", {
  expect_error(
    delete_files("temp6.txt", data_folder = folder, user_confirm = FALSE)
  )
})

test_that("Expected files are deleted", {

  expect_false({
    delete_files(files[1], data_folder = folder, user_confirm = FALSE)
    file.exists(filepaths[1])
  })

  expect_false({
    delete_files(".txt", data_folder = folder, user_confirm = FALSE)
    any(file.exists(filepaths))
  })

})

unlink(folder, recursive = TRUE)

## test-data-import.R

context("Data import")

test_that("an 'excelFile' object can be created", {
  expect_error(excelFile(NULL), "argument is of length zero")
  expect_error(excelFile("file.doc"))
  expect_error(excelFile("anothefile.txt"), "Expected an MS Excel file ")
})

## test-spreadsheet.R

context("Spreadsheet operations")

test_that("Headers can be identified", {
  xlObj <- excelfile("test-dir/test-file-2.xlsx")
  df <- xlObj$data[[1]]
  header <-
    c("name",
      "email",
      "address",
      "phone",
      "BIRTHDAY AND WED ANN",
      "how do you know")
  locator <- locate_header(df, header)

  expect_is(locator, "header-locator")
  expect_s3_class(locator, "header-locator")
  expect_error(locate_header())
  expect_error(locate_header(matrix(1:12, nrow = 3), hdr = letters[1:4]))
  expect_error(locate_header(df, hdr = 1:5))
  expect_output(locate_header(df, header, quietly = FALSE))
})

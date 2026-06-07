test_that("remove_numbers removes numbers", {
  expect_equal(
    remove_numbers("inflation increased by 5 percent in 2024"),
    "inflation increased by   percent in  "
  )

  expect_equal(
    remove_numbers(c("abc123", "45def")),
    c("abc ", " def")
  )
})

test_that("remove_numbers preserves NA values", {
  expect_equal(
    remove_numbers(c("abc123", NA)),
    c("abc ", NA)
  )
})

test_that("remove_numbers errors on non-character input", {
  expect_error(remove_numbers(123))
  expect_error(remove_numbers(TRUE))
})

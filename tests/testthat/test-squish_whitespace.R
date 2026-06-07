test_that("squish_whitespace removes repeated whitespace", {
  expect_equal(
    squish_whitespace("inflation    growth   employment"),
    "inflation growth employment"
  )

  expect_equal(
    squish_whitespace(c("  a   b  ", "c\t\t d")),
    c("a b", "c d")
  )
})

test_that("squish_whitespace preserves NA values", {
  expect_equal(
    squish_whitespace(c("  hello   world  ", NA)),
    c("hello world", NA)
  )
})

test_that("squish_whitespace errors on non-character input", {
  expect_error(squish_whitespace(123))
  expect_error(squish_whitespace(TRUE))
})

test_that("preprocessing functions can be used together", {
  texts <- c(
    "INFLATION, increased by 5.2% in 2024!",
    "GROWTH   remained: stable."
  )

  out <- texts |>
    lowercase() |>
    remove_punctuation() |>
    remove_numbers() |>
    squish_whitespace()

  expect_equal(
    out,
    c(
      "inflation increased by in",
      "growth remained stable"
    )
  )
})

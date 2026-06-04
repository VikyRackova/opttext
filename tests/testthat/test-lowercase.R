test_that("lowercase converts uppercase to lowercase", {
  expect_equal(
    lowercase("HELLO WORLD"),
    "hello world"
  )
})

test_that("lowercase handles a vector of strings", {
  input <- c("HELLO", "WORLD", "Hello World")
  expected <- c("hello", "world", "hello world")

  expect_equal(
    lowercase(input),
    expected
  )
})

test_that("lowercase preserves punctuation and numbers", {
  expect_equal(
    lowercase("GDP Growth 2024: +3.5%!"),
    "gdp growth 2024: +3.5%!"
  )
})

test_that("lowercase handles Unicode correctly", {
  expect_equal(
    lowercase("ČESKÁ REPUBLIKA"),
    "česká republika"
  )
})

test_that("lowercase handles multiple Unicode scripts", {
  expect_equal(
    lowercase("ÁÉÍÓÚ"),
    "áéíóú"
  )

  expect_equal(
    lowercase("Straße"),
    "straße"
  )

  expect_equal(
    lowercase("ΕΛΛΑΔΑ"),
    "ελλαδα"
  )
})

test_that("NAs are handled as NAs", {
  expect_equal(
    lowercase(c("HELLO", NA_character_)),
    c("hello", NA_character_)
  )
})

test_that("lowercase handles empty strings", {
  expect_equal(
    lowercase(""),
    ""
  )
})

test_that("lowercase throws error on non-character input", {
  expect_error(
    lowercase(123),
    "Input must be a character"
  )

  expect_error(
    lowercase(TRUE),
    "Input must be a character"
  )

  expect_error(
    lowercase(list("HELLO")),
    "Input must be a character"
  )
})

test_that("lowercase throws error on empty input", {
  expect_error(
    lowercase(character(0)),
    "Input cannot be empty"
  )
})

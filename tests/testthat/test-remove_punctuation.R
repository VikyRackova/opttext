test_that("remove_punctuation removes punctuation", {
  expect_equal(
    remove_punctuation("hello, world!"),
    "hello  world "
  )

  expect_equal(
    remove_punctuation(c("a.b", "c?d")),
    c("a b", "c d")
  )
})

test_that("remove_punctuation preserves NA values", {
  expect_equal(
    remove_punctuation(c("hello!", NA)),
    c("hello ", NA)
  )
})

test_that("remove_punctuation errors on non-character input", {
  expect_error(remove_punctuation(123))
  expect_error(remove_punctuation(TRUE))
})

test_that("fast_dtm creates a fastDtm object", {
  text <- c("apple apple banana", "banana orange")

  dtm <- fast_dtm(text, stopwords = FALSE)

  expect_s4_class(dtm, "fastDtm")
  expect_equal(dim(dtm), c(2, 3))
  expect_equal(dtm@vocabulary, c("apple", "banana", "orange"))
})

test_that("fast_dtm counts terms correctly", {

  dtm <- fast_dtm(
    c(
      "apple banana apple",
      "banana orange"
    ),
    stopwords = FALSE
  )

  expected <- matrix(
    c(
      2, 1, 0,
      0, 1, 1
    ),
    nrow = 2,
    byrow = TRUE,
    dimnames = list(
      NULL,
      c("apple", "banana", "orange")
    )
  )

  expect_equal(as.matrix(dtm), expected)
  expect_equal(dtm@term_frequency, c(2, 2, 1))
  expect_equal(dtm@document_frequency, c(1, 2, 1))

})

test_that("fast_dtm stores metadata", {
  df <- data.frame(
    Text = c("apple banana", "orange banana"),
    Date = as.Date(c("2024-01-01", "2024-01-02"))
  )

  dtm <- fast_dtm(df, texts = "Text", metadata = "Date", stopwords = FALSE)

  expect_equal(dtm@docvars$Date, df$Date)
})

test_that("fast_dtm rejects punctuation", {
  expect_error(
    fast_dtm(c("apple, banana"), stopwords = FALSE),
    "punctuation"
  )
})

test_that("fast_dtm rejects uppercase letters", {
  expect_error(
    fast_dtm(c("Apple banana"), stopwords = FALSE),
    "uppercase"
  )
})

test_that("fast_dtm removes NA documents with warning", {
  text <- c("apple banana", NA, "orange banana")

  expect_warning(
    dtm <- fast_dtm(text, stopwords = FALSE),
    "Removed 1 document"
  )

  expect_equal(nrow(dtm), 2)
})

test_that("fast_dtm removes punctuation when requested", {
  text <- c("hello, world!", "growth: inflation.")

  dtm <- fast_dtm(
    text,
    stopwords = FALSE,
    remove_punct = TRUE
  )

  expect_true("hello" %in% dtm@vocabulary)
  expect_true("world" %in% dtm@vocabulary)
  expect_true("growth" %in% dtm@vocabulary)
  expect_true("inflation" %in% dtm@vocabulary)

  expect_false(any(grepl("[[:punct:]]", dtm@vocabulary)))
})

test_that("fast_dtm errors on punctuation by default", {
  expect_error(
    fast_dtm("hello, world", stopwords = FALSE),
    "contains punctuation"
  )
})

test_that("fast_dtm stores multiple metadata columns", {
  df <- data.frame(
    Text = c("apple banana", "orange banana"),
    Date = as.Date(c("2024-01-01", "2024-01-02")),
    Source = c("a", "b")
  )

  dtm <- fast_dtm(
    df,
    texts = "Text",
    metadata = c("Date", "Source"),
    stopwords = FALSE
  )

  expect_equal(dtm@docvars$Date, df$Date)
  expect_equal(dtm@docvars$Source, df$Source)
})

test_that("fast_dtm removes metadata rows when text is NA", {
  df <- data.frame(
    Text = c("apple banana", NA, "orange banana"),
    Date = as.Date(c("2024-01-01", "2024-01-02", "2024-01-03")),
    Source = c("a", "b", "c")
  )

  expect_warning(
    dtm <- fast_dtm(
      df,
      texts = "Text",
      metadata = c("Date", "Source"),
      stopwords = FALSE
    ),
    "Removed 1 document"
  )

  expect_equal(nrow(dtm), 2)
  expect_equal(dtm@docvars$Source, c("a", "c"))
  expect_equal(dtm@docvars$Date, as.Date(c("2024-01-01", "2024-01-03")))
})

test_that("fast_dtm removes stopwords when requested", {
  text <- c("the apple and the banana")

  dtm <- fast_dtm(text, stopwords = TRUE)

  expect_false("the" %in% dtm@vocabulary)
  expect_false("and" %in% dtm@vocabulary)
  expect_true("apple" %in% dtm@vocabulary)
  expect_true("banana" %in% dtm@vocabulary)
})

test_that("fast_dtm validates data frame inputs", {
  df <- data.frame(Text = c("apple banana"))

  expect_error(
    fast_dtm(df),
    "`texts` must be supplied"
  )

  expect_error(
    fast_dtm(df, texts = "Missing"),
    "`texts` was not found"
  )

  expect_error(
    fast_dtm(df, texts = "Text", metadata = "Missing"),
    "All `metadata` must be columns"
  )
})

test_that("dtm_trim returns a fastDtm object", {
  texts <- c(
    "inflation growth",
    "inflation market",
    "growth market"
  )

  dtm <- fast_dtm(texts, stopwords = FALSE)
  trimmed <- dtm_trim(dtm, min_docfreq = 1)

  expect_s4_class(trimmed, "fastDtm")
  expect_true(inherits(trimmed, "dgCMatrix"))
})

test_that("dtm_trim trims by minimum document frequency", {
  texts <- c(
    "inflation growth",
    "inflation market",
    "growth market",
    "rare"
  )

  dtm <- fast_dtm(texts, stopwords = FALSE)
  trimmed <- dtm_trim(dtm, min_docfreq = 2)

  expect_equal(
    sort(trimmed@vocabulary),
    c("growth", "inflation", "market")
  )

  expect_false("rare" %in% trimmed@vocabulary)
})

test_that("dtm_trim trims by minimum term frequency", {
  texts <- c(
    "inflation inflation growth",
    "market growth",
    "rare"
  )

  dtm <- fast_dtm(texts, stopwords = FALSE)
  trimmed <- dtm_trim(dtm, min_termfreq = 2)

  expect_equal(
    sort(trimmed@vocabulary),
    c("growth", "inflation")
  )

  expect_false("market" %in% trimmed@vocabulary)
  expect_false("rare" %in% trimmed@vocabulary)
})

test_that("dtm_trim trims by both document frequency and term frequency", {
  texts <- c(
    "inflation inflation growth",
    "inflation market",
    "growth market",
    "rare"
  )

  dtm <- fast_dtm(texts, stopwords = FALSE)

  trimmed <- dtm_trim(
    dtm,
    min_docfreq = 2,
    min_termfreq = 2
  )

  expect_equal(
    sort(trimmed@vocabulary),
    c("growth", "inflation", "market")
  )

  expect_false("rare" %in% trimmed@vocabulary)
})

test_that("dtm_trim keeps document metadata", {
  df <- data.frame(
    Text = c(
      "inflation growth",
      "inflation market",
      "growth market",
      "rare"
    ),
    Date = as.Date(c(
      "2024-01-01",
      "2024-01-02",
      "2024-01-03",
      "2024-01-04"
    ))
  )

  dtm <- fast_dtm(
    data = df,
    texts = "Text",
    metadata = "Date",
    stopwords = FALSE
  )

  trimmed <- dtm_trim(dtm, min_docfreq = 2)

  expect_equal(
    trimmed@docvars,
    dtm@docvars
  )
})

test_that("dtm_trim updates frequency slots correctly", {
  texts <- c(
    "inflation inflation growth",
    "inflation market",
    "growth market",
    "rare"
  )

  dtm <- fast_dtm(texts, stopwords = FALSE)
  trimmed <- dtm_trim(dtm, min_docfreq = 2)

  expect_equal(
    trimmed@term_frequency,
    as.numeric(Matrix::colSums(trimmed))
  )

  expect_equal(
    trimmed@document_frequency,
    as.numeric(Matrix::colSums(trimmed > 0))
  )

  expect_equal(
    trimmed@vocabulary,
    colnames(trimmed)
  )
})

test_that("dtm_trim errors on non-fastDtm input", {
  expect_error(
    dtm_trim(matrix(1:4, nrow = 2)),
    "`dtm` must be a fastDtm object"
  )
})

test_that("dtm_trim errors on invalid trimming arguments", {
  texts <- c(
    "inflation growth",
    "inflation market"
  )

  dtm <- fast_dtm(texts, stopwords = FALSE)

  expect_error(dtm_trim(dtm, min_docfreq = 0))
  expect_error(dtm_trim(dtm, min_docfreq = -1))
  expect_error(dtm_trim(dtm, min_termfreq = 0))
  expect_error(dtm_trim(dtm, min_termfreq = -1))
})

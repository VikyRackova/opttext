df <- data.frame(
    text  = c(
      "inflation rose as central banks raised interest rates",
      "monetary policy tightened amid persistent inflationary pressures",
      "interest rates increased for the third consecutive quarter",
      "parliament debated the new housing policy amid public protests",
      "prime minister announced reform package for public sector wages",
      "election campaign intensified as polls showed narrow margins",
      "renewable energy investment surpassed fossil fuels for first time",
      "climate summit failed to reach binding agreement on emissions",
      "carbon emissions fell as renewable capacity expanded rapidly",
      "extreme weather events increased frequency linked to warming"
    ),
    date   = as.Date(c(
      "2021-03-01", "2021-09-01", "2022-02-01", "2022-06-01",
      "2022-11-01", "2023-03-01", "2023-07-01", "2023-11-01",
      "2024-02-01", "2024-07-01"
    )),
    topic  = rep(c("economics", "politics", "climate"), times = c(3, 3, 4)),
    source = c("FT", "Reuters", "BBC", "Guardian", "Times",
               "FT", "Reuters", "BBC", "Guardian", "Times"),
    stringsAsFactors = FALSE
  )


dtm_base  <- fast_dtm(df, texts = "text", metadata = c("date", "topic", "source"))
dtm_nodv  <- fast_dtm(c(
  "inflation rose sharply last quarter",
  "monetary policy remained accommodative",
  "parliament debated the new housing bill"
))


test_that(".default_facet_ncol returns correct column counts", {
  expect_equal(opttext:::.default_facet_ncol(1L),  2L)
  expect_equal(opttext:::.default_facet_ncol(4L),  2L)
  expect_equal(opttext:::.default_facet_ncol(5L),  3L)
  expect_equal(opttext:::.default_facet_ncol(9L),  3L)
  expect_equal(opttext:::.default_facet_ncol(10L), 4L)
  expect_equal(opttext:::.default_facet_ncol(20L), 4L)
})



test_that("plot_top_terms returns a ggplot object", {
  p <- plot_top_terms(dtm_base)
  expect_s3_class(p, "ggplot")
})

test_that("plot_top_terms returns a ggplot object with group", {
  p <- plot_top_terms(dtm_base, group = "topic")
  expect_s3_class(p, "ggplot")
})

test_that("plot_top_terms respects n argument", {
  p   <- plot_top_terms(dtm_base, n = 5L)
  dat <- p$data
  expect_lte(nrow(dat), 5L)
})

test_that("plot_top_terms respects n per group", {
  p   <- plot_top_terms(dtm_base, n = 3L, group = "topic")
  dat <- p$data
  counts <- table(dat$group)
  expect_true(all(counts <= 3L))
})

test_that("plot_top_terms accepts custom facet_ncol", {
  p <- plot_top_terms(dtm_base, group = "topic", facet_ncol = 1L)
  expect_s3_class(p, "ggplot")
  facet_params <- p$facet$params
  expect_equal(facet_params$ncol, 1L)
})

test_that("plot_top_terms passes ... to geom_col", {
  expect_no_error(plot_top_terms(dtm_base, width = 0.5))
})

test_that("plot_top_terms errors on non-fastDtm input", {
  expect_error(plot_top_terms(matrix(1:4, 2, 2)), "`dtm` must be a `fastDtm` object")
})

test_that("plot_top_terms errors on non-positive n", {
  expect_error(plot_top_terms(dtm_base, n = 0L),  "`n` must be a positive integer")
  expect_error(plot_top_terms(dtm_base, n = -1L), "`n` must be a positive integer")
  expect_error(plot_top_terms(dtm_base, n = "5"), "`n` must be a positive integer")
})

test_that("plot_top_terms errors when group column is absent", {
  expect_error(
    plot_top_terms(dtm_base, group = "nonexistent"),
    "column 'nonexistent' was not found"
  )
})

test_that("plot_top_terms errors when docvars is empty and group is supplied", {
  expect_error(
    plot_top_terms(dtm_nodv, group = "topic"),
    "`@docvars` is empty"
  )
})

test_that("plot_top_terms is extensible with ggplot2 +", {
  p <- plot_top_terms(dtm_base) + ggplot2::theme_bw()
  expect_s3_class(p, "ggplot")
})






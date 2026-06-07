make_dtm <- function() {
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
  fast_dtm(df, texts = "text", metadata = c("date", "topic", "source"))
}

dtm_base  <- make_dtm()
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



test_that("plot_doc_lengths returns a ggplot object", {
  suppressWarnings(p <- plot_doc_lengths(dtm_base))
  expect_s3_class(p, "ggplot")
})

test_that("plot_doc_lengths returns a ggplot with group", {
  suppressWarnings(p <- plot_doc_lengths(dtm_base, group = "topic"))
  expect_s3_class(p, "ggplot")
})

test_that("plot_doc_lengths returns a ggplot with facet = TRUE", {
  suppressWarnings(p <- plot_doc_lengths(dtm_base, group = "topic", facet = TRUE))
  expect_s3_class(p, "ggplot")
  expect_s3_class(p$facet, "FacetWrap")
})

test_that("plot_doc_lengths warns for small corpora", {
  expect_warning(
    plot_doc_lengths(dtm_base),
    "may not be informative"
  )
})

test_that("plot_doc_lengths does not warn for corpora >= 50 documents", {
  big_texts <- rep(
    c("inflation rose sharply", "parliament debated new policy",
      "renewable energy investment surpassed fossil fuels"),
    times = 20L
  )
  big_dtm <- fast_dtm(big_texts)
  expect_no_warning(plot_doc_lengths(big_dtm))
})

test_that("plot_doc_lengths respects bins argument", {
  suppressWarnings(p <- plot_doc_lengths(dtm_base, bins = 10L))
  built <- ggplot2::ggplot_build(p)
  expect_lte(length(unique(built$data[[1]]$x)), 10L + 1L)
})

test_that("plot_doc_lengths errors on non-fastDtm input", {
  expect_error(plot_doc_lengths(list()), "`dtm` must be a `fastDtm` object")
})

test_that("plot_doc_lengths errors on non-positive bins", {
  expect_error(
    suppressWarnings(plot_doc_lengths(dtm_base, bins = 0L)),
    "`bins` must be a positive integer"
  )
})

test_that("plot_doc_lengths errors on non-logical facet", {
  expect_error(
    suppressWarnings(plot_doc_lengths(dtm_base, facet = "yes")),
    "`facet` must be TRUE or FALSE"
  )
})

test_that("plot_doc_lengths errors when group column is absent", {
  expect_error(
    suppressWarnings(plot_doc_lengths(dtm_base, group = "missing_col")),
    "column 'missing_col' was not found"
  )
})

test_that("plot_doc_lengths errors when docvars is empty and group supplied", {
  expect_error(
    suppressWarnings(plot_doc_lengths(dtm_nodv, group = "topic")),
    "`@docvars` is empty"
  )
})

test_that("plot_doc_lengths accepts custom facet_ncol", {
  suppressWarnings(
    p <- plot_doc_lengths(dtm_base, group = "topic", facet = TRUE, facet_ncol = 1L)
  )
  expect_equal(p$facet$params$ncol, 1L)
})

test_that("plot_doc_lengths is extensible with ggplot2 +", {
  suppressWarnings(p <- plot_doc_lengths(dtm_base) + ggplot2::theme_bw())
  expect_s3_class(p, "ggplot")
})



test_that("plot_term_heatmap returns a ggplot object", {
  p <- plot_term_heatmap(dtm_base, date = "date")
  expect_s3_class(p, "ggplot")
})

test_that("plot_term_heatmap returns a ggplot with group", {
  p <- plot_term_heatmap(dtm_base, date = "date", group = "topic")
  expect_s3_class(p, "ggplot")
  expect_s3_class(p$facet, "FacetWrap")
})

test_that("plot_term_heatmap respects n argument", {
  p   <- plot_term_heatmap(dtm_base, date = "date", n = 5L)
  dat <- p$data
  expect_lte(length(unique(dat$term)), 5L)
})

test_that("plot_term_heatmap works for by = 'year'", {
  expect_s3_class(plot_term_heatmap(dtm_base, date = "date", by = "year"), "ggplot")
})

test_that("plot_term_heatmap works for by = 'quarter'", {
  expect_s3_class(plot_term_heatmap(dtm_base, date = "date", by = "quarter"), "ggplot")
})

test_that("plot_term_heatmap works for by = 'month'", {
  expect_s3_class(plot_term_heatmap(dtm_base, date = "date", by = "month"), "ggplot")
})

test_that("plot_term_heatmap relative frequencies sum to <= 1 per period", {
  p   <- plot_term_heatmap(dtm_base, date = "date", by = "year")
  dat <- p$data
  period_sums <- tapply(dat$rel_freq, dat$period, sum)
  expect_true(all(period_sums <= 1 + 1e-9))
})

test_that("plot_term_heatmap relative frequencies are non-negative", {
  p <- plot_term_heatmap(dtm_base, date = "date", by = "year")
  expect_true(all(p$data$rel_freq >= 0))
})

test_that("plot_term_heatmap accepts custom facet_ncol", {
  p <- plot_term_heatmap(dtm_base, date = "date", group = "topic", facet_ncol = 1L)
  expect_equal(p$facet$params$ncol, 1L)
})

test_that("plot_term_heatmap errors on non-fastDtm input", {
  expect_error(
    plot_term_heatmap(data.frame(), date = "date"),
    "`dtm` must be a `fastDtm` object"
  )
})

test_that("plot_term_heatmap errors when date column is absent", {
  expect_error(
    plot_term_heatmap(dtm_base, date = "missing_date"),
    "column 'missing_date' was not found"
  )
})

test_that("plot_term_heatmap errors when date column is not a date class", {
  df2 <- data.frame(
    text     = c("inflation rose", "parliament debated policy"),
    not_date = c("2022-01-01", "2023-06-01"),
    stringsAsFactors = FALSE
  )
  dtm2 <- fast_dtm(df2, texts = "text", metadata = "not_date")
  expect_error(
    plot_term_heatmap(dtm2, date = "not_date"),
    "must be of class Date"
  )
})

test_that("plot_term_heatmap errors on invalid by argument", {
  expect_error(
    plot_term_heatmap(dtm_base, date = "date", by = "week"),
    "should be one of"
  )
})

test_that("plot_term_heatmap errors on non-positive n", {
  expect_error(
    plot_term_heatmap(dtm_base, date = "date", n = 0L),
    "`n` must be a positive integer"
  )
})

test_that("plot_term_heatmap errors when docvars is empty", {
  expect_error(
    plot_term_heatmap(dtm_nodv, date = "date"),
    "`@docvars` is empty"
  )
})

test_that("plot_term_heatmap is extensible with ggplot2 +", {
  p <- plot_term_heatmap(dtm_base, date = "date") +
    ggplot2::scale_fill_viridis_c(option = "plasma")
  expect_s3_class(p, "ggplot")
})

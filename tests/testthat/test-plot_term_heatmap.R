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

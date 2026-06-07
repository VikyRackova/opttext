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


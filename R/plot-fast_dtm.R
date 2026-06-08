utils::globalVariables(c("frequency", "period", "rel_freq", "term", "term_ord"))
#' Validate a @docvars column name
#'
#' @param dtm A \code{fastDtm} object.
#' @param col Character string; the column name to look up.
#' @param arg Character string; the argument name used in the error message.
#' @return The column vector, invisibly.
#' @noRd
.check_docvar <- function(dtm, col, arg) {
  if (!is.character(col) || length(col) != 1L) {
    stop(sprintf("`%s` must be a single character string.", arg), call. = FALSE)
  }

  if (nrow(dtm@docvars) == 0L) {
    stop(sprintf("`%s` was supplied but `@docvars` is empty.", arg), call. = FALSE)
  }

  if (!col %in% names(dtm@docvars)) {
    stop(
      sprintf("`%s` column '%s' was not found in `@docvars`.", arg, col),
      call. = FALSE
    )
  }

  invisible(dtm@docvars[[col]])
}


#' Validate a positive integer argument
#'
#' @param x Argument value.
#' @param arg Argument name.
#' @return Integer value.
#' @noRd
.check_positive_integer <- function(x, arg) {
  if (!is.numeric(x) || length(x) != 1L || is.na(x) || x < 1L) {
    stop(sprintf("`%s` must be a positive integer.", arg), call. = FALSE)
  }

  as.integer(x)
}


#' Choose a sensible number of facet columns
#'
#' @param n_groups Number of groups.
#' @return Integer number of facet columns.
#' @noRd
.default_facet_ncol <- function(n_groups) {
  if (n_groups <= 4L) return(2L)
  if (n_groups <= 9L) return(3L)
  4L
}


#' Plot the Top-N Most Frequent Terms
#'
#' Produces a horizontal bar chart of the \code{n} most frequent terms in a
#' \code{fastDtm} object. When \code{group} is supplied, the matrix is split by
#' the selected \code{@docvars} column and per-group term frequencies are shown
#' in facets, each with its own top-\code{n} ranking.
#'
#' The function returns a \code{ggplot} object, so all aesthetic and theme
#' adjustments can be added with \code{+}, for example:
#' \code{+ ggplot2::theme_bw()}, \code{+ ggplot2::labs(title = "My title")},
#' or \code{+ ggplot2::scale_fill_viridis_d(option = "plasma")}.
#'
#' @param dtm A \code{fastDtm} object returned by \code{\link{fast_dtm}}.
#' @param n Positive integer. Number of top terms to display. Defaults to
#'   \code{20}.
#' @param group Optional character string naming a column in \code{@docvars}
#'   to facet by. The column must exist in \code{@docvars}.
#' @param facet_ncol Number of facet columns when \code{group} is supplied.
#'   If \code{NULL}, a sensible default is chosen based on the number of groups.
#' @param ... Additional arguments passed to \code{ggplot2::geom_col()}.
#'
#' @return A \code{ggplot} object.
#'
#' @seealso \code{\link{fast_dtm}}, \code{\link{dtm_trim}},
#'   \code{\link{plot_doc_lengths}}, \code{\link{plot_term_heatmap}}
#'
#' @examples
#' text <- c(
#'   "inflation increased during the quarter",
#'   "economic growth remained stable",
#'   "inflation growth important indicators"
#' )
#'
#' dtm <- fast_dtm(text)
#'
#' plot_top_terms(dtm)
#' plot_top_terms(dtm, n = 5)
#'
#' # Customise with standard ggplot2 syntax:
#' plot_top_terms(dtm, fill = "grey40") +
#'   ggplot2::theme_bw() +
#'   ggplot2::labs(title = "Custom title")
#'
#' @importFrom ggplot2 ggplot aes geom_col coord_flip facet_wrap
#'   scale_fill_viridis_d scale_x_discrete labs theme_minimal theme element_text
#' @importFrom Matrix colSums
#' @export
plot_top_terms <- function(dtm,
                           n          = 20L,
                           group      = NULL,
                           facet_ncol = NULL,
                           ...) {

  if (!inherits(dtm, "fastDtm")) {
    stop("`dtm` must be a `fastDtm` object.", call. = FALSE)
  }

  n <- .check_positive_integer(n, "n")

  if (!is.null(facet_ncol)) {
    facet_ncol <- .check_positive_integer(facet_ncol, "facet_ncol")
  }

  if (is.null(group)) {

    freqs   <- dtm@term_frequency
    vocab   <- dtm@vocabulary
    top_idx <- order(freqs, decreasing = TRUE)[seq_len(min(n, length(vocab)))]

    plot_df      <- data.frame(
      term      = vocab[top_idx],
      frequency = freqs[top_idx],
      stringsAsFactors = FALSE
    )
    plot_df$term <- factor(plot_df$term, levels = rev(plot_df$term))

    p <- ggplot2::ggplot(plot_df, ggplot2::aes(x = term, y = frequency)) +
      ggplot2::geom_col(fill = "#3B82F6", ...) +
      ggplot2::coord_flip() +
      ggplot2::labs(
        title = sprintf("Top %d terms by corpus frequency", n),
        x     = NULL,
        y     = "Term frequency"
      ) +
      ggplot2::theme_minimal() +
      ggplot2::theme(plot.title = ggplot2::element_text(face = "bold"))

  } else {

    grp_vec    <- .check_docvar(dtm, group, "group")
    levels_grp <- unique(grp_vec)

    if (is.null(facet_ncol)) {
      facet_ncol <- .default_facet_ncol(length(levels_grp))
    }

    dfs <- lapply(levels_grp, function(g) {
      rows      <- which(grp_vec == g)
      sub_mat   <- dtm[rows, , drop = FALSE]
      freqs     <- Matrix::colSums(sub_mat)
      nonzero_n <- sum(freqs > 0)

      if (nonzero_n == 0L) return(NULL)

      top_idx <- order(freqs, decreasing = TRUE)[seq_len(min(n, nonzero_n))]

      data.frame(
        term      = dtm@vocabulary[top_idx],
        frequency = as.numeric(freqs[top_idx]),
        group     = as.character(g),
        stringsAsFactors = FALSE
      )
    })

    dfs <- Filter(Negate(is.null), dfs)

    if (length(dfs) == 0L) {
      stop("No non-zero term frequencies found for the supplied groups.", call. = FALSE)
    }

    plot_df          <- do.call(rbind, dfs)
    plot_df$term_ord <- stats::reorder(
      interaction(plot_df$term, plot_df$group, drop = TRUE),
      plot_df$frequency
    )

    p <- ggplot2::ggplot(
      plot_df,
      ggplot2::aes(x = term_ord, y = frequency, fill = group)
    ) +
      ggplot2::geom_col(show.legend = FALSE, ...) +
      ggplot2::coord_flip() +
      ggplot2::facet_wrap(~ group, scales = "free_y", ncol = facet_ncol) +
      ggplot2::scale_fill_viridis_d(option = "D", begin = 0.2, end = 0.8) +
      ggplot2::scale_x_discrete(labels = function(x) sub("\\..*$", "", x)) +
      ggplot2::labs(
        title = sprintf("Top %d terms per group", n),
        x     = NULL,
        y     = "Term frequency",
        fill  = group
      ) +
      ggplot2::theme_minimal() +
      ggplot2::theme(
        plot.title = ggplot2::element_text(face = "bold"),
        strip.text = ggplot2::element_text(face = "bold")
      )
  }

  p
}


#' Plot the Distribution of Document Lengths
#'
#' Displays a histogram of per-document token counts, calculated as the row
#' sums of the document-term matrix. When \code{group} is supplied, the
#' distribution is shown as overlapping semi-transparent histograms coloured
#' by group. Set \code{facet = TRUE} to show groups in separate panels
#' instead.
#'
#' The function returns a \code{ggplot} object, so all aesthetic and theme
#' adjustments can be added with \code{+}, for example:
#' \code{+ ggplot2::theme_bw()} or \code{+ ggplot2::scale_fill_brewer()}.
#'
#' @param dtm A \code{fastDtm} object returned by \code{\link{fast_dtm}}.
#' @param bins Positive integer. Number of histogram bins. Defaults to
#'   \code{30}.
#' @param group Optional character string naming a column in \code{@docvars}
#'   to colour by. The column must exist in \code{@docvars}.
#' @param facet Logical. When \code{group} is supplied, should groups be shown
#'   in separate panels (\code{TRUE}) or as overlapping histograms
#'   (\code{FALSE})? Defaults to \code{FALSE}.
#' @param facet_ncol Optional positive integer. Number of facet columns when
#'   \code{facet = TRUE}. If \code{NULL}, a sensible default is chosen based
#'   on the number of groups.
#' @param ... Additional arguments passed to \code{ggplot2::geom_histogram()}.
#'
#' @return A \code{ggplot} object.
#'
#' @seealso \code{\link{fast_dtm}}, \code{\link{plot_top_terms}},
#'   \code{\link{plot_term_heatmap}}
#'
#' @examples
#' text <- c(
#'   "inflation increased during the quarter",
#'   "economic growth remained stable",
#'   "inflation growth important indicators"
#' )
#'
#' dtm <- fast_dtm(text)
#'
#' plot_doc_lengths(dtm)
#' plot_doc_lengths(dtm, bins = 10)
#'
#' # Customise with standard ggplot2 syntax:
#' plot_doc_lengths(dtm) +
#'   ggplot2::theme_bw() +
#'   ggplot2::labs(title = "Custom document length plot")
#'
#' @importFrom ggplot2 ggplot aes geom_histogram facet_wrap scale_fill_viridis_d
#'   labs theme_minimal theme element_text
#' @importFrom Matrix rowSums
#' @export
plot_doc_lengths <- function(dtm,
                             bins       = 30L,
                             group      = NULL,
                             facet      = FALSE,
                             facet_ncol = NULL,
                             ...) {

  if (!inherits(dtm, "fastDtm")) {
    stop("`dtm` must be a `fastDtm` object.", call. = FALSE)
  }

  bins <- .check_positive_integer(bins, "bins")

  if (!is.logical(facet) || length(facet) != 1L) {
    stop("`facet` must be TRUE or FALSE.", call. = FALSE)
  }

  if (!is.null(facet_ncol)) {
    facet_ncol <- .check_positive_integer(facet_ncol, "facet_ncol")
  }

  doc_lengths <- Matrix::rowSums(dtm)

  if (length(doc_lengths) < 50L) {
    warning(
      sprintf(
        "`dtm` has only %d document(s); the document length histogram may not be informative.",
        length(doc_lengths)
      ),
      call. = FALSE
    )
  }

  if (is.null(group)) {

    plot_df <- data.frame(length = as.numeric(doc_lengths))

    p <- ggplot2::ggplot(plot_df, ggplot2::aes(x = length)) +
      ggplot2::geom_histogram(
        bins      = bins,
        fill      = "#3B82F6",
        colour    = "white",
        linewidth = 0.2,
        ...
      ) +
      ggplot2::labs(
        title = "Distribution of document lengths",
        x     = "Tokens per document",
        y     = "Count"
      ) +
      ggplot2::theme_minimal() +
      ggplot2::theme(plot.title = ggplot2::element_text(face = "bold"))

  } else {

    grp_vec    <- .check_docvar(dtm, group, "group")
    levels_grp <- unique(grp_vec)

    if (is.null(facet_ncol)) {
      facet_ncol <- .default_facet_ncol(length(levels_grp))
    }

    plot_df <- data.frame(
      length = as.numeric(doc_lengths),
      group  = as.character(grp_vec),
      stringsAsFactors = FALSE
    )

    p <- ggplot2::ggplot(plot_df, ggplot2::aes(x = length, fill = group)) +
      ggplot2::geom_histogram(
        bins      = bins,
        colour    = "white",
        linewidth = 0.2,
        alpha     = if (facet) 1 else 0.6,
        position  = if (facet) "stack" else "identity",
        ...
      ) +
      ggplot2::scale_fill_viridis_d(option = "D", begin = 0.2, end = 0.8) +
      ggplot2::labs(
        title = "Distribution of document lengths by group",
        x     = "Tokens per document",
        y     = "Count",
        fill  = group
      ) +
      ggplot2::theme_minimal() +
      ggplot2::theme(
        plot.title = ggplot2::element_text(face = "bold"),
        strip.text = ggplot2::element_text(face = "bold")
      )

    if (facet) {
      p <- p + ggplot2::facet_wrap(~ group, scales = "free_y", ncol = facet_ncol)
    }
  }

  p
}


#' Plot a Term-Frequency Heatmap Over Time
#'
#' Aggregates the counts of the top-\code{n} terms over time periods and
#' displays them as a heatmap, with terms on the y-axis and time on the x-axis.
#' The colour encodes each term's share of all tokens in that period, making
#' periods with different corpus sizes more comparable.
#'
#' Requires at least one \code{Date}, \code{POSIXct}, or \code{POSIXlt} column
#' in \code{@docvars}, passed through the \code{date} argument.
#'
#' The function returns a \code{ggplot} object, so all aesthetic and theme
#' adjustments can be added with \code{+}, for example:
#' \code{+ ggplot2::theme_bw()}, \code{+ ggplot2::labs(title = "My title")},
#' or \code{+ ggplot2::scale_fill_viridis_c(option = "plasma")}.
#'
#' @param dtm A \code{fastDtm} object.
#' @param date Character string naming the \code{Date}/\code{POSIXct}/
#'   \code{POSIXlt} column in \code{@docvars} to use as the time axis.
#' @param n Positive integer. Number of top terms, by overall frequency, to
#'   display.
#' @param by Character string controlling date aggregation. One of
#'   \code{"year"}, \code{"quarter"}, or \code{"month"}.
#' @param group Optional character string naming a column in \code{@docvars}
#'   to facet by.
#' @param facet_ncol Optional positive integer. Number of facet columns when
#'   \code{group} is supplied. If \code{NULL}, a sensible default is chosen
#'   based on the number of groups.
#' @param ... Additional arguments passed to \code{ggplot2::geom_tile()}.
#'
#' @return A \code{ggplot} object.
#'
#' @seealso \code{\link{fast_dtm}}, \code{\link{plot_top_terms}},
#'   \code{\link{plot_doc_lengths}}
#'
#' @examples
#' df <- data.frame(
#'   text = c(
#'     "inflation increased during the quarter",
#'     "economic growth remained stable",
#'     "inflation growth important indicators"
#'   ),
#'   date   = as.Date(c("2022-03-01", "2023-07-01", "2024-01-15")),
#'   source = c("a", "b", "a")
#' )
#'
#' dtm <- fast_dtm(df, texts = "text", metadata = c("date", "source"))
#'
#' plot_term_heatmap(dtm, date = "date")
#' plot_term_heatmap(dtm, date = "date", group = "source")
#'
#' # Customise with standard ggplot2 syntax:
#' plot_term_heatmap(dtm, date = "date") +
#'   ggplot2::theme_bw() +
#'   ggplot2::labs(title = "Custom heatmap")
#'
#' @importFrom ggplot2 ggplot aes geom_tile scale_fill_viridis_c facet_wrap
#'   labs theme_minimal theme element_text element_blank
#' @importFrom Matrix colSums
#' @export
plot_term_heatmap <- function(dtm,
                              date,
                              n          = 20L,
                              by         = c("year", "quarter", "month"),
                              group      = NULL,
                              facet_ncol = NULL,
                              ...) {

  if (!inherits(dtm, "fastDtm")) {
    stop("`dtm` must be a `fastDtm` object.", call. = FALSE)
  }

  n  <- .check_positive_integer(n, "n")
  by <- match.arg(by)

  if (!is.null(facet_ncol)) {
    facet_ncol <- .check_positive_integer(facet_ncol, "facet_ncol")
  }

  date_vec <- .check_docvar(dtm, date, "date")

  if (!inherits(date_vec, c("Date", "POSIXct", "POSIXlt"))) {
    stop(
      sprintf("Column '%s' must be of class Date, POSIXct, or POSIXlt.", date),
      call. = FALSE
    )
  }

  date_vec   <- as.Date(date_vec)
  period_vec <- switch(
    by,
    year    = format(date_vec, "%Y"),
    quarter = paste0(
      format(date_vec, "%Y"), " Q",
      ceiling(as.integer(format(date_vec, "%m")) / 3)
    ),
    month   = format(date_vec, "%Y-%m")
  )

  top_idx   <- order(dtm@term_frequency, decreasing = TRUE)[
    seq_len(min(n, length(dtm@vocabulary)))
  ]
  top_terms <- dtm@vocabulary[top_idx]
  sub_mat   <- dtm[, top_idx, drop = FALSE]

  .build_heatmap_df <- function(mat, periods) {
    uperiods <- sort(unique(periods))
    do.call(rbind, lapply(uperiods, function(p) {
      rows     <- which(periods == p)
      col_sums <- Matrix::colSums(mat[rows, , drop = FALSE])
      total    <- sum(col_sums)
      rel_freq <- if (total > 0) col_sums / total else col_sums
      data.frame(
        period   = p,
        term     = top_terms,
        rel_freq = as.numeric(rel_freq),
        stringsAsFactors = FALSE
      )
    }))
  }

  if (is.null(group)) {

    plot_df      <- .build_heatmap_df(sub_mat, period_vec)
    plot_df$term <- factor(plot_df$term, levels = rev(top_terms))

    p <- ggplot2::ggplot(
      plot_df,
      ggplot2::aes(x = period, y = term, fill = rel_freq)
    ) +
      ggplot2::geom_tile(colour = "white", linewidth = 0.3, ...) +
      ggplot2::scale_fill_viridis_c(
        option = "magma", begin = 0.1, end = 0.95,
        labels = scales::label_percent(accuracy = 0.1)
      ) +
      ggplot2::labs(
        title = sprintf("Top %d terms over time (%s)", n, by),
        x     = NULL,
        y     = NULL,
        fill  = "Relative\nfrequency"
      ) +
      ggplot2::theme_minimal() +
      ggplot2::theme(
        plot.title  = ggplot2::element_text(face = "bold"),
        axis.text.x = ggplot2::element_text(angle = 45, hjust = 1),
        panel.grid  = ggplot2::element_blank()
      )

  } else {

    grp_vec    <- .check_docvar(dtm, group, "group")
    grp_levels <- sort(unique(as.character(grp_vec)))

    if (is.null(facet_ncol)) {
      facet_ncol <- .default_facet_ncol(length(grp_levels))
    }

    dfs <- lapply(grp_levels, function(g) {
      rows <- which(as.character(grp_vec) == g)
      df   <- .build_heatmap_df(sub_mat[rows, , drop = FALSE], period_vec[rows])
      df$group <- g
      df
    })

    plot_df       <- do.call(rbind, dfs)
    plot_df$term  <- factor(plot_df$term,  levels = rev(top_terms))
    plot_df$group <- factor(plot_df$group, levels = grp_levels)

    p <- ggplot2::ggplot(
      plot_df,
      ggplot2::aes(x = period, y = term, fill = rel_freq)
    ) +
      ggplot2::geom_tile(colour = "white", linewidth = 0.3, ...) +
      ggplot2::scale_fill_viridis_c(
        option = "magma", begin = 0.1, end = 0.95,
        labels = scales::label_percent(accuracy = 0.1)
      ) +
      ggplot2::facet_wrap(~ group, ncol = facet_ncol) +
      ggplot2::labs(
        title = sprintf("Top %d terms over time by group (%s)", n, by),
        x     = NULL,
        y     = NULL,
        fill  = "Relative\nfrequency"
      ) +
      ggplot2::theme_minimal() +
      ggplot2::theme(
        plot.title  = ggplot2::element_text(face = "bold"),
        strip.text  = ggplot2::element_text(face = "bold"),
        axis.text.x = ggplot2::element_text(angle = 45, hjust = 1),
        panel.grid  = ggplot2::element_blank()
      )
  }

  p
}

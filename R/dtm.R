#' fastDtm Class
#'
#' An S4 class extending \code{\linkS4class{dgCMatrix}} for storing
#' document-term matrices together with vocabulary statistics and
#' document-level metadata.
#'
#' @slot vocabulary Character vector of unique terms corresponding to matrix
#'   columns, sorted alphabetically.
#' @slot term_frequency Numeric vector of corpus-wide term frequencies. Each
#'   element gives the total count of the corresponding term across all
#'   documents.
#' @slot document_frequency Numeric vector giving the number of documents in
#'   which each term appears at least once.
#' @slot docvars Data frame of document-level metadata retained from the
#'   \code{metadata} argument of \code{\link{fast_dtm}}. An empty data frame
#'   when no metadata was supplied.
#'
#' @seealso \code{\link{fast_dtm}}, \code{\link{dtm_trim}}
#'
#' @name fastDtm-class
#' @importClassesFrom Matrix dgCMatrix
#' @importFrom methods new setClass
#' @export
setClass(
  "fastDtm",
  contains = "dgCMatrix",
  slots = c(
    vocabulary = "character",
    term_frequency = "numeric",
    document_frequency = "numeric",
    docvars = "data.frame"
  )
)

#' Construct a Fast Document-Term Matrix
#'
#' Creates a sparse document-term matrix directly from pre-cleaned text. The
#' result is returned as a \code{\link{fastDtm-class}} object, which extends
#' \code{\linkS4class{dgCMatrix}} and is compatible with downstream
#' sparse-matrix workflows including topic modelling.
#'
#' @param data A character vector with one document per element, or a data
#'   frame containing a text column.
#' @param texts Character string naming the column in \code{data} that contains
#'   the document text. Required when \code{data} is a data frame; ignored when
#'   \code{data} is a character vector.
#' @param metadata Optional character vector naming columns in \code{data} to
#'   retain as document-level metadata in the \code{@docvars} slot. Ignored
#'   when \code{data} is a character vector.
#' @param stopwords Logical. Should stopwords be removed before constructing
#'   the matrix? Defaults to \code{TRUE}.
#' @param language Character string specifying the stopword language passed to
#'   \code{\link[stopwords]{stopwords}} with \code{source = "snowball"}.
#'   Defaults to \code{"en"}.
#' @param remove_punct Logical. Should Unicode punctuation be removed before
#'   constructing the matrix? Defaults to \code{FALSE}. When \code{FALSE}, the
#'   function stops with an informative error if punctuation is detected.
#' @param threads Positive integer specifying the number of threads used by
#'   \pkg{RcppParallel}. When \code{NULL} (default), the number of threads is
#'   determined automatically by \pkg{RcppParallel}.
#'
#' @details
#' \code{fast_dtm()} is optimised for already-cleaned text and avoids creating
#' an intermediate token object. The input text must satisfy three
#' preprocessing conditions before calling this function:
#'
#' \itemize{
#'   \item All letters must be lowercase.
#'   \item Unicode punctuation must be removed (unless \code{remove_punct = TRUE}).
#'   \item Terms must be separated by whitespace.
#' }
#'
#' Input validation is performed using \pkg{stringi}. The function stops with
#' an informative error if uppercase Unicode letters or punctuation are detected
#' and \code{remove_punct = FALSE}.
#'
#' Missing text values (\code{NA}) are silently removed before matrix
#' construction. A warning is issued reporting the number of removed documents.
#' When metadata is supplied, the corresponding rows are also removed so the
#' matrix and \code{@docvars} remain aligned.
#'
#' When \code{stopwords = TRUE}, the Snowball stopword list for the selected
#' language is retrieved via \code{\link[stopwords]{stopwords}}.
#'
#' The returned \code{fastDtm} object contains the sparse count matrix and four
#' additional S4 slots:
#'
#' \itemize{
#'   \item \code{@vocabulary}: unique terms corresponding to matrix columns,
#'     sorted alphabetically.
#'   \item \code{@term_frequency}: corpus-wide count of each term.
#'   \item \code{@document_frequency}: number of documents containing each
#'     term.
#'   \item \code{@docvars}: data frame of document-level metadata.
#' }
#'
#' The matrix rows correspond to documents and columns correspond to unique
#' terms. Row names are set to the names of the input character vector or the
#' row names of the input data frame, when available.
#'
#' @return An object of class \code{\link{fastDtm-class}}, extending
#'   \code{\linkS4class{dgCMatrix}}. Rows correspond to documents and
#'   columns correspond to unique terms sorted alphabetically.
#'
#' @seealso
#' \code{\link{dtm_trim}} for removing infrequent terms,
#' \code{\link{lowercase}} for Unicode-aware lowercasing,
#' \code{\link{remove_punctuation}} for removing punctuation,
#' \code{\link[topicmodels]{LDA}} for topic modelling,
#' \code{\link[stopwords]{stopwords}} for stopword lists.
#'
#' @examples
#' texts <- c(
#'   "inflation increased during the quarter",
#'   "economic growth remained stable",
#'   "inflation growth important indicators")
#'
#' dtm <- fast_dtm(texts)
#'
#' dtm@vocabulary
#' dtm@term_frequency
#' dtm@document_frequency
#'
#' \dontrun{
#' # Data frame input with metadata
#' df <- data.frame(
#'   Text   = texts,
#'   Date   = as.Date(c("2024-01-01", "2024-02-01", "2024-03-01")),
#'   Source = c("a", "b", "c")
#' )
#'
#' dtm_df <- fast_dtm(
#'   data     = df,
#'   texts    = "Text",
#'   metadata = c("Date", "Source")
#' )
#'
#' dtm_df@docvars
#'
#' # Automatic punctuation removal
#' text_punct <- c(
#'   "inflation, increased during the quarter.",
#'   "economic growth remained stable!"
#' )
#'
#' fast_dtm(text_punct, remove_punct = TRUE)
#' }
#'
#' @importFrom methods new
#' @importFrom Matrix sparseMatrix colSums
#' @importFrom stringi stri_detect_charclass stri_replace_all_charclass
#' @export
fast_dtm <- function(data,
                     texts = NULL,
                     metadata = NULL,
                     stopwords = TRUE,
                     language = "en",
                     remove_punct = FALSE,
                     threads = NULL) {

  if (is.data.frame(data)) {

    if (is.null(texts)) {
      stop("`texts` must be supplied when `data` is a data frame.", call. = FALSE)
    }

    if (!texts %in% names(data)) {
      stop("`texts` was not found in `data`.", call. = FALSE)
    }

    if (!is.null(metadata) && !all(metadata %in% names(data))) {
      stop("All `metadata` must be columns in `data`.", call. = FALSE)
    }

    text <- data[[texts]]
    doc_names <- rownames(data)

    docvars <- if (!is.null(metadata)) {
      as.data.frame(data[metadata], stringsAsFactors = FALSE, check.names = FALSE)
    } else {
      data.frame()
    }

  } else {

    text <- data
    doc_names <- names(text)
    docvars <- data.frame()

  }

  if (!is.character(text)) {
    stop("The text input must be a character vector.", call. = FALSE)
  }

  if (anyNA(text)) {
    keep <- !is.na(text)
    n_removed <- sum(!keep)

    text <- text[keep]

    if (!is.null(doc_names)) {
      doc_names <- doc_names[keep]
    }

    if (nrow(docvars) > 0) {
      docvars <- as.data.frame(docvars[keep, , drop = FALSE])
      rownames(docvars) <- NULL
    }

    warning(
      sprintf(
        "Removed %s document(s) containing missing text values.",
        n_removed
      ),
      call. = FALSE
    )
  }

  if (!is.logical(remove_punct) || length(remove_punct) != 1) {
    stop("`remove_punct` must be TRUE or FALSE.", call. = FALSE)
  }

  if (remove_punct) {
    text <- stringi::stri_replace_all_charclass(text, "\\p{P}", " ")
  } else {
    if (any(stringi::stri_detect_charclass(text, "\\p{P}"))) {
      stop(
        paste(
          "The text contains punctuation.",
          "Use `remove_punct = TRUE` or remove punctuation before using `fast_dtm()`."
        ),
        call. = FALSE
      )
    }
  }

  if (any(stringi::stri_detect_charclass(text, "\\p{Lu}"))) {
    stop(
      paste(
        "The text contains uppercase letters.",
        "Please lowercase the text before using `fast_dtm()`."
      ),
      call. = FALSE
    )
  }

  if (!is.logical(stopwords) || length(stopwords) != 1) {
    stop("`stopwords` must be TRUE or FALSE.", call. = FALSE)
  }

  if (!is.character(language) || length(language) != 1) {
    stop("`language` must be a single character value.", call. = FALSE)
  }

  if (!is.null(threads)) {
    if (!is.numeric(threads) || length(threads) != 1 || threads < 1) {
      stop("`threads` must be a positive number.", call. = FALSE)
    }

    RcppParallel::setThreadOptions(numThreads = threads)
  }

  stopword_vec <- if (stopwords) {
    stopwords::stopwords(language, source = "snowball")
  } else {
    character(0)
  }

  out <- fast_dtm_cpp(
    text = text,
    stopwords = stopword_vec,
    remove_stopwords = stopwords
  )

  mat <- Matrix::sparseMatrix(
    i = out$i,
    j = out$j,
    x = out$x,
    dims = c(length(text), length(out$vocab)),
    dimnames = list(doc_names, out$vocab)
  )

  new(
    "fastDtm",
    mat,
    vocabulary = colnames(mat),
    term_frequency = as.numeric(Matrix::colSums(mat)),
    document_frequency = as.numeric(Matrix::colSums(mat > 0)),
    docvars = docvars
  )
}


#' Trim a fastDtm Object
#'
#' Removes infrequent terms from a \code{\link{fastDtm-class}} object by
#' applying minimum document-frequency and term-frequency thresholds.
#'
#' @param dtm A \code{fastDtm} object returned by \code{\link{fast_dtm}}.
#' @param min_docfreq Positive integer. Minimum number of documents a term must
#'   appear in to be retained. Defaults to \code{1} (no filtering by document
#'   frequency).
#' @param min_termfreq Positive integer. Minimum corpus-wide count a term must
#'   have to be retained. Defaults to \code{1} (no filtering by term
#'   frequency).
#'
#' @details
#' A term is retained only if it satisfies both thresholds simultaneously. All
#' S4 slots are updated to reflect the trimmed matrix:
#'
#' \itemize{
#'   \item \code{@vocabulary}: updated to the retained terms.
#'   \item \code{@term_frequency}: recomputed from the trimmed matrix.
#'   \item \code{@document_frequency}: recomputed from the trimmed matrix.
#'   \item \code{@docvars}: unchanged; rows correspond to the same documents.
#' }
#'
#' Trimming is useful for removing noise before topic modelling or other
#' sparse-matrix methods, and for reducing memory usage on large corpora.
#'
#' @return A \code{\link{fastDtm-class}} object with the same number of rows
#'   (documents) as \code{dtm} but with infrequent terms removed from the
#'   columns.
#'
#' @seealso \code{\link{fast_dtm}} for constructing the matrix,
#'   \code{\link{plot_top_terms}} for visualising term frequencies.
#'
#' @examples
#' texts <- c(
#'   "inflation increased during the quarter",
#'   "economic growth remained stable",
#'   "inflation growth important indicators"
#' )
#'
#' dtm <- fast_dtm(texts)
#'
#' # Retain only terms appearing in at least 2 documents
#' dtm_trim(dtm, min_docfreq = 2)
#'
#' # Retain only terms with a corpus frequency of at least 2
#' dtm_trim(dtm, min_termfreq = 2)
#'
#' # Apply both thresholds simultaneously
#' dtm_trim(dtm, min_docfreq = 2, min_termfreq = 2)
#'
#' @importFrom methods new
#' @importFrom Matrix colSums
#' @export
dtm_trim <- function(dtm,
                     min_docfreq = 1,
                     min_termfreq = 1) {

  if (!inherits(dtm, "fastDtm")) {
    stop("`dtm` must be a fastDtm object.", call. = FALSE)
  }

  min_docfreq  <- .check_positive_integer(min_docfreq,  "min_docfreq")
  min_termfreq <- .check_positive_integer(min_termfreq, "min_termfreq")

  keep <- dtm@document_frequency >= min_docfreq &
    dtm@term_frequency >= min_termfreq

  mat <- dtm[, keep, drop = FALSE]

  methods::new(
    "fastDtm",
    mat,
    vocabulary         = dtm@vocabulary[keep],
    term_frequency     = as.numeric(Matrix::colSums(mat)),
    document_frequency = as.numeric(Matrix::colSums(mat > 0)),
    docvars            = dtm@docvars
  )
}

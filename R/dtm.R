#' fastDtm Class
#'
#' An S4 class extending \code{\linkS4class{dgCMatrix}} for storing
#' document-term matrices together with vocabulary statistics and
#' document-level metadata.
#'
#' @slot vocabulary Character vector of unique terms.
#' @slot term_frequency Corpus-wide term frequencies.
#' @slot document_frequency Number of documents containing each term.
#' @slot docvars Data frame of document-level metadata.
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
#' Creates a sparse document-term matrix directly from pre-cleaned text. The result is
#' returned as a \code{fastDtm} object.
#'
#' A \code{fastDtm} object extends \code{\linkS4class{dgCMatrix}}, so it can
#' be used directly with functions that accept sparse document-term matrices,
#' including \code{\link[topicmodels]{LDA}}.
#'
#' @param data A character vector with one document per element, or a data
#'   frame containing a text column.
#' @param texts Character string naming the column in \code{data} that contains
#'   the document text. Required when \code{data} is a data frame; ignored when
#'   \code{data} is a character vector.
#' @param metadata Optional character vector naming columns in \code{data} to
#'   retain as document-level metadata. Ignored when \code{data} is a character
#'   vector.
#' @param stopwords Logical. Should stopwords be removed? Defaults to
#'   \code{TRUE}.
#' @param language Character string specifying the stopword language. Passed to
#'   \code{stopwords::stopwords()} with \code{source = "snowball"}.
#'   Defaults to \code{"en"}.
#' @param remove_punct Logical. Should punctuation be removed before constructing
#'   the document-term matrix? Defaults to \code{FALSE}. If \code{FALSE}, the
#'   function stops when punctuation is detected.
#' @param threads Integer specifying the number of threads used by
#'   \pkg{RcppParallel}. If \code{NULL}, the number of threads is determined
#'   automatically by \pkg{RcppParallel}.
#'
#' @details
#' \code{fast_dtm()} is optimized for already processed text and avoids
#' creating an intermediate tokens object. The input text must satisfy three
#' preprocessing conditions:
#'
#' \itemize{
#'   \item text must be lowercase;
#'   \item Unicode punctuation must be removed, unless \code{remove_punct = TRUE};
#'   \item terms must be separated by whitespace.
#' }
#'
#' Unicode validation is performed with \pkg{stringi}. The function stops with
#' an informative error if uppercase Unicode letters or Unicode punctuation are
#' detected.
#'
#' Missing text values are removed before matrix construction. If any
#' \code{NA} documents are removed, a warning reports the number of removed
#' documents. Metadata rows are removed accordingly, so the document-term matrix
#' and document-level metadata remain aligned.
#'
#' If \code{stopwords = TRUE}, stopwords are retrieved from the
#' \pkg{stopwords} package using the Snowball list for the selected language.
#'
#' The returned object contains the sparse matrix and four additional S4 slots:
#'
#' \itemize{
#'   \item \code{@vocabulary}: unique terms corresponding to matrix columns;
#'   \item \code{@term_frequency}: corpus-wide count of each term;
#'   \item \code{@document_frequency}: number of documents containing each term;
#'   \item \code{@docvars}: document-level metadata retained from
#'     \code{metadata}.
#' }
#'
#' @return An object of class \code{fastDtm}, extending
#'   \code{\linkS4class{dgCMatrix}}. Rows correspond to documents and columns
#'   correspond to unique terms sorted alphabetically.
#'
#' @seealso
#' \code{\linkS4class{dgCMatrix}},
#' \code{\link[topicmodels]{LDA}},
#' \code{\link[stopwords]{stopwords}},
#' \code{\link[stringi]{stri_detect_charclass}}
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
#' df <- data.frame(
#'   Text = texts,
#'   Date = as.Date(c("2024-01-01", "2024-02-01", "2024-03-01")),
#'   Source = c("a", "b", "c")
#' )
#'
#' dtm_df <- fast_dtm(
#'   data = df,
#'   texts = "Text",
#'   metadata = c("Date", "Source"))
#'
#' dtm_df@docvars
#'
#' text_punct <- c(
#'   "inflation, increased during the quarter.",
#'   "economic growth remained stable!"
#' )
#'
#' fast_dtm(text_punct, remove_punct = TRUE)
#'}
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
#' Removes infrequent terms from a fastDtm object.
#'
#' @param dtm A fastDtm object.
#' @param min_docfreq Minimum document frequency.
#' @param min_termfreq Minimum corpus term frequency.
#'
#' @return A trimmed fastDtm object.
#'
#' @export
dtm_trim <- function(dtm,
                     min_docfreq = 1,
                     min_termfreq = 1) {

  if (!inherits(dtm, "fastDtm")) {
    stop("`dtm` must be a fastDtm object.", call. = FALSE)
  }

  min_docfreq <- .check_positive_integer(min_docfreq, "min_docfreq")
  min_termfreq <- .check_positive_integer(min_termfreq, "min_termfreq")

  keep <- dtm@document_frequency >= min_docfreq &
    dtm@term_frequency >= min_termfreq

  mat <- dtm[, keep, drop = FALSE]

  methods::new(
    "fastDtm",
    mat,
    vocabulary = dtm@vocabulary[keep],
    term_frequency = as.numeric(Matrix::colSums(mat)),
    document_frequency = as.numeric(Matrix::colSums(mat > 0)),
    docvars = dtm@docvars
  )
}

#' Convert Text to Lowercase
#'
#' Converts all uppercase letters in a character string or character vector to
#' lowercase using a parallel vocabulary-caching strategy with Unicode-aware
#' normalisation via the ICU library.
#'
#' @param texts A character string or character vector whose elements will be
#'   converted to lowercase.
#'
#' @details
#' \code{lowercase()} is designed for efficient text standardisation prior to
#' tokenisation, document-term matrix construction, or other natural language
#' processing tasks.
#'
#' The function uses a two-pass approach: unique tokens are lowercased once and
#' cached, and document reconstruction is performed in parallel using
#' \pkg{RcppParallel}. For ASCII-only input, a fast bit-flip operation is
#' applied. For input containing non-ASCII characters, Unicode-aware
#' lowercasing is performed via the ICU library, correctly handling accented
#' characters, ligatures, and other Unicode letter forms.
#'
#' The function preserves the length and ordering of the input vector. Missing
#' values (\code{NA}) are retained and returned unchanged.
#'
#' @return A character vector of the same length as \code{texts}, with all
#'   alphabetic characters converted to lowercase. The structure and ordering
#'   of the input are preserved. \code{NA} elements are returned as \code{NA}.
#'
#' @seealso
#' \code{\link{remove_punctuation}}, \code{\link{remove_numbers}},
#' \code{\link{squish_whitespace}}, \code{\link{fast_dtm}}
#'
#' @examples
#' # Single string
#' lowercase("HELLO WORLD")
#'
#' # Character vector
#' lowercase(c("APPLE", "BaNaNa", "Cherry"))
#'
#' # Unicode characters are handled correctly
#' lowercase(c("ÜNÏCÖDÉ", "ΕΛΛΗΝΙΚΆ", "ČESKY"))
#'
#' # Text containing punctuation (preserved)
#' lowercase("INFLATION, GROWTH, AND EMPLOYMENT.")
#'
#' # Missing values are preserved
#' lowercase(c("HELLO", NA, "WORLD"))
#'
#' @export
lowercase <- function(texts) {
  if (!is.character(texts)) stop("Input must be a character string or vector")
  if (length(texts) == 0)   stop("Input cannot be empty")

  out <- texts
  not_na <- !is.na(texts)

  out[not_na] <- cpp_lowercase(texts[not_na])
  out
}


#' Remove Punctuation
#'
#' Removes all Unicode punctuation characters from a character string or
#' character vector, replacing them with a single space.
#'
#' @param texts A character string or character vector to process.
#'
#' @details
#' Punctuation removal is performed using \pkg{stringi} with the Unicode
#' property class \code{\\p{P}}, which matches all Unicode punctuation
#' characters including ASCII punctuation, quotation marks, brackets,
#' dashes, and other punctuation across scripts.
#'
#' Punctuation characters are replaced with a space rather than an empty
#' string to avoid inadvertently merging adjacent words. Use
#' \code{\link{squish_whitespace}} afterwards to collapse any resulting
#' multiple spaces.
#'
#' @return A character vector of the same length as \code{texts}, with all
#'   Unicode punctuation replaced by a single space.
#'
#' @seealso
#' \code{\link{lowercase}}, \code{\link{remove_numbers}},
#' \code{\link{squish_whitespace}}, \code{\link{fast_dtm}}
#'
#' @examples
#' # Basic usage
#' remove_punctuation("Inflation, growth, and employment.")
#'
#' # Multiple punctuation types
#' remove_punctuation("Q1: growth was 'stable' (see table 1).")
#'
#' # Character vector
#' remove_punctuation(c("Hello, world!", "No punctuation here"))
#'
#' # Combine with squish_whitespace to clean up extra spaces
#' squish_whitespace(remove_punctuation("Hello, world!"))
#'
#' @export
remove_punctuation <- function(texts) {

  if (!is.character(texts)) {
    stop("Input must be a character vector.", call. = FALSE)
  }

  stringi::stri_replace_all_charclass(
    texts,
    "\\p{P}",
    " "
  )
}


#' Remove Numbers
#'
#' Removes all numeric digit sequences from a character string or character
#' vector, replacing them with a single space.
#'
#' @param texts A character string or character vector to process.
#'
#' @details
#' Digit sequences are matched using the regular expression \code{[0-9]+} via
#' \pkg{stringi} and replaced with a single space. This removes Arabic digit
#' sequences but does not affect number words (e.g., "three") or digits in
#' other scripts.
#'
#' Digits are replaced with a space rather than an empty string to avoid
#' inadvertently merging adjacent words. Use \code{\link{squish_whitespace}}
#' afterwards to collapse any resulting multiple spaces.
#'
#' @return A character vector of the same length as \code{texts}, with all
#'   digit sequences replaced by a single space.
#'
#' @seealso
#' \code{\link{lowercase}}, \code{\link{remove_punctuation}},
#' \code{\link{squish_whitespace}}, \code{\link{fast_dtm}}
#'
#' @examples
#' # Basic usage
#' remove_numbers("Inflation increased by 5 percent in 2024")
#'
#' # Multiple numbers
#' remove_numbers("Q1 2024: growth of 3.2 percent")
#'
#' # Character vector
#' remove_numbers(c("GDP grew 2 percent", "No numbers here"))
#'
#' # Combine with squish_whitespace
#' squish_whitespace(remove_numbers("Growth of 3 percent in 2024"))
#'
#' @export
remove_numbers <- function(texts) {

  if (!is.character(texts)) {
    stop("Input must be a character vector.", call. = FALSE)
  }

  stringi::stri_replace_all_regex(
    texts,
    "[0-9]+",
    " "
  )
}


#' Squish Whitespace
#'
#' Removes leading and trailing whitespace and collapses all internal sequences
#' of whitespace to a single space.
#'
#' @param texts A character string or character vector to process.
#'
#' @details
#' Whitespace normalisation is performed in two steps using \pkg{stringi}:
#' leading and trailing whitespace is removed with
#' \code{\link[stringi]{stri_trim_both}}, then internal whitespace sequences
#' (spaces, tabs, newlines) are collapsed to a single space using a regex
#' replacement.
#'
#' This function is particularly useful as a final cleaning step after
#' \code{\link{remove_punctuation}} or \code{\link{remove_numbers}}, both of
#' which replace matched characters with a space and may leave multiple
#' consecutive spaces.
#'
#' @return A character vector of the same length as \code{texts}, with leading
#'   and trailing whitespace removed and internal whitespace collapsed to a
#'   single space.
#'
#' @seealso
#' \code{\link{lowercase}}, \code{\link{remove_punctuation}},
#' \code{\link{remove_numbers}}, \code{\link{fast_dtm}}
#'
#' @examples
#' # Collapse internal spaces
#' squish_whitespace("inflation    growth   employment")
#'
#' # Remove leading and trailing whitespace
#' squish_whitespace("  hello world  ")
#'
#' # Typical use after remove_punctuation
#' squish_whitespace(remove_punctuation("Inflation, growth, and employment."))
#'
#' # Full preprocessing pipeline
#' texts <- c("  INFLATION, Increased 5%  ", "GROWTH remained stable in 2024.")
#' texts |>
#'   lowercase() |>
#'   remove_punctuation() |>
#'   remove_numbers() |>
#'   squish_whitespace()
#'
#' @export
squish_whitespace <- function(texts) {

  if (!is.character(texts)) {
    stop("Input must be a character vector.", call. = FALSE)
  }

  texts <- stringi::stri_trim_both(texts)

  stringi::stri_replace_all_regex(
    texts,
    "\\s+",
    " "
  )
}

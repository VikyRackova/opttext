#' Convert Text to Lowercase
#'
#' \code{lowercase()} converts all uppercase letters in a character string or character vector
#' to lowercase.
#'
#' @param texts A character string or character vector whose elements will be
#'   converted to lowercase.
#'
#' @details
#' \code{lowercase()} is designed for efficient text standardization prior to
#' tokenization, document-term matrix construction, or other natural language
#' processing tasks.
#'
#' The function preserves the length and ordering of the input vector. Missing
#' values (\code{NA}) are retained and returned unchanged.
#'
#' @return A character string or character vector of the same length as
#'   \code{texts}, with all alphabetic characters converted to lowercase.
#'
#' @examples
#' # Single string
#' lowercase("HELLO WORLD")
#'
#' # Character vector
#' lowercase(c("APPLE", "BaNaNa", "Cherry"))
#'
#' # Text containing punctuation
#' lowercase("INFLATION, GROWTH, AND EMPLOYMENT")
#'
#' # Missing values are preserved
#' lowercase(c("HELLO", NA))
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
#' Removes all Unicode punctuation characters from text.
#'
#' @param texts Character vector.
#'
#' @return Character vector.
#'
#' @examples
#' remove_punctuation("Inflation, growth, and employment.")
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
#' Removes numeric characters from text.
#'
#' @param texts Character vector.
#'
#' @return Character vector.
#'
#' @examples
#' remove_numbers("Inflation increased by 5 percent")
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
#' Removes leading and trailing whitespace and converts repeated
#' whitespace to a single space.
#'
#' @param texts Character vector.
#'
#' @return Character vector.
#'
#' @examples
#' squish_whitespace("inflation    growth   employment")
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

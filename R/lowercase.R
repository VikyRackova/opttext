#' Convert Text to Lowercase
#'
#' Convert all uppercase letters in a character string or character vector
#' to lowercase.
#'
#' @usage
#' lowercase(text)
#'
#' @param text A character string or character vector whose elements will be
#'   converted to lowercase.
#'
#' @return A character string or character vector of the same length as
#'   \code{text}, with all alphabetic characters converted to lowercase.
#'
#' @details
#' This function is useful for text preprocessing and standardization.
#'
#'
#' @examples
#' # Single string
#' lowercase("HELLO WORLD")
#'
#' # Character vector
#' lowercase(c("APPLE", "BaNaNa", "Cherry"))
#'
#'
#'
#' @useDynLib opttext, .registration = TRUE
#' @export
lowercase <- function(texts) {
  if (!is.character(texts)) stop("Input must be a character string or vector")
  if (length(texts) == 0)   stop("Input cannot be empty")

  out <- texts
  not_na <- !is.na(texts)

  out[not_na] <- cpp_lowercase(texts[not_na])
  out
}

#' Tokenize
#'
#' @param string A charactrer vector with one element
#' @inheritParams stringr::str_split
#'
#' @returns A character vector
#' @export
#'
#' @examples
#' x <- "alpha, beta, gamma, delta"
#' str_split_one(x,pattern = ",")
#' str_split_one(x,pattern = ",", n = 2)


lowercase_tokenize <- function(text) {
  text_lower <- tolower(text)
  tokens <- unlist(strsplit(text_lower, "\\W+"))
  tokens <- tokens[tokens != ""]  # remove empty strings
  return(tokens)
}

lowercase_tokenize("Hello World! This is an Example.")

library(tokenizers)

lowercase_tokenize <- function(text) {
  tokenize_words(tolower(text), lowercase = FALSE)[[1]]
}

library(dplyr)
FED_Minutes<- FED_Minutes%>%
  arrange(Date)

library(quanteda)

lowercase_tokenize <- function(text) {
  tokens(tolower(text), remove_punct = TRUE)[[1]]
}


usethis::use_r("FED_Minutes")
devtools::document()

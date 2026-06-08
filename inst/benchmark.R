library(microbenchmark)
library(opttext)
library(stringi)
library(quanteda)
library(Matrix)


Text_short <- rep("THE FED INCREASED INTEREST RATES IN RESPONSE TO INFLATION 2024!!!",100000)
FED_sentences <- unlist(
  strsplit(FED_Minutes$Text, "(?<=[.!?])\\s+", perl = TRUE))
Text_docs <- FED_Minutes$Text
Text_docs_lower<-lowercase(FED_Minutes$Text)

# Lowercase: artificial short repeated text
Benchmark_lowercase_short <- microbenchmark(
  opttext = lowercase(Text_short),
  base_R = tolower(Text_short),
  stringi = stringi::stri_trans_tolower(Text_short),
  times = 20L)

print(Benchmark_lowercase_short)



# Lowercase: FED sentence-level text
Benchmark_lowercase_sentences <- microbenchmark(
  opttext = lowercase(FED_sentences),
  base_R = tolower(FED_sentences),
  stringi = stringi::stri_trans_tolower(FED_sentences),
  times = 20L)

print(Benchmark_lowercase_sentences)



# Remove punctuation
Benchmark_remove_punctuation <- microbenchmark(
  opttext = remove_punctuation(Text_short),
  base_R = gsub("[[:punct:]]+", " ", Text_short),
  stringi = stringi::stri_replace_all_regex(Text_short, "[[:punct:]]+", " "),
  quanteda = as.character(
    quanteda::tokens(Text_short, remove_punct = TRUE)
  ),
  times = 20L)

print(Benchmark_remove_punctuation)


# Remove numbers
Benchmark_remove_numbers <- microbenchmark(
  opttext = remove_numbers(Text_short),
  base_R = gsub("[[:digit:]]+", " ", Text_short),
  stringi = stringi::stri_replace_all_regex(Text_short, "[[:digit:]]+", " "),
  quanteda = as.character(
    quanteda::tokens(Text_short, remove_numbers = TRUE)
  ),
  times = 20L)

print(Benchmark_remove_numbers)




#  Full preprocessing pipeline
Benchmark_pipeline <- microbenchmark(
  opttext = {
    x <- lowercase(Text_short)
    x <- remove_punctuation(x)
    x <- remove_numbers(x)
    x
  },
  base_R = {
    x <- tolower(Text_short)
    x <- gsub("[[:punct:]]+", " ", x)
    x <- gsub("[[:digit:]]+", " ", x)
    x <- strsplit(x, "\\s+")
    x <- lapply(x, function(words) {
      paste(words[!words %in% Stopwords], collapse = " ")
    })
    unlist(x)
  },
  quanteda = {
    quanteda::tokens(
      Text_short,
      remove_punct = TRUE,
      remove_numbers = TRUE
    ) |>
      quanteda::tokens_tolower() |>
      quanteda::tokens_remove(pattern = Stopwords)
  },
  times = 20L
)

print(Benchmark_pipeline)



# fast_dtm: FED documents
Benchmark_fast_dtm <- microbenchmark(
  opttext = fast_dtm(Text_docs_lower, remove_punct = TRUE),
  quanteda = {
    quanteda::tokens(Text_docs_lower, remove_punct = TRUE) |>
      quanteda::dfm()
  },
  times = 20L
)

print(Benchmark_fast_dtm)




#  dtm_trim
Dtm_opttext <- fast_dtm(Text_docs_lower, remove_punct = TRUE)

Dfm_quanteda <- quanteda::tokens(Text_docs_lower, remove_punct = TRUE) |>
  quanteda::dfm()

Benchmark_dtm_trim <- microbenchmark(
  opttext = dtm_trim(
    Dtm_opttext,
    min_docfreq = 2,
    min_termfreq = 2
  ),
  quanteda = quanteda::dfm_trim(
    Dfm_quanteda,
    min_docfreq = 2,
    min_termfreq = 2
  ),
  times = 20L
)

print(Benchmark_dtm_trim)




#  Collect summaries
Benchmark_summary <- list(
  lowercase_short = summary(Benchmark_lowercase_short),
  lowercase_sentences = summary(Benchmark_lowercase_sentences),
  remove_punctuation = summary(Benchmark_remove_punctuation),
  remove_numbers = summary(Benchmark_remove_numbers),
  preprocessing_pipeline = summary(Benchmark_pipeline),
  fast_dtm_documents = summary(Benchmark_fast_dtm),
  dtm_trim = summary(Benchmark_dtm_trim)
)

Benchmark_summary


